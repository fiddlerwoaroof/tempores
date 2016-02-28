;;;; timesheet.lisp

(in-package #:timesheet)

(ubiquitous:restore 'timesheet)

;;; "timesheet" goes here. Hacks and glory await!

(defvar *default-time-sheet-file*)
(defvar *rate*)

(defclass status-calculator ()
  ((rate :initarg :rate :accessor rate)
   (total-hours :initform 0 :initarg :total-hour :accessor total-hours)
   (client-totals :initarg :client-totals :accessor client-totals)))

(defclass status-line ()
  ((client :initarg :client :accessor client)
   (duration :initarg :duration :accessor duration :initform 0)))

(defclass parsed-entry ()
  ((date :initarg :date :accessor date)
   (client :initarg :client :accessor client)
   (memo :initarg :memo :accessor memo)))

(defclass complete-entry (parsed-entry)
  ((duration :initarg :duration :accessor duration)))

(defclass partial-entry (parsed-entry)
  ((start-times :initarg :start-times :initform nil :accessor start-times)))

(defcondition incomplete-entry-warning (warning) ())
(defmethod duration ((obj partial-entry))
  (warn "incomplete entry detected for ~a" (client obj))
  (local-time-duration:duration))

(defun make-complete-entry (date client memo duration)
  (make-instance 'complete-entry
                 :date date
                 :client client
                 :memo memo
                 :duration duration))

(defun make-partial-entry (date client memo start-times)
  (make-instance 'partial-entry
                 :date date
                 :client client
                 :memo memo
                 :start-times start-times))

(define-condition parsing-error ()
  ((leftovers :initarg :leftovers :accessor leftovers))
  (:report (lambda (condition stream)
               (format stream "Parse error: ~20s leftover" (leftovers condition)))))

(defun parse-file (&optional (file *default-time-sheet-file*))
  (with-open-file (s file :direction :input)
    (let ((dest (make-string (file-length s))))
      (read-sequence dest s)
      (multiple-value-bind (parsed leftovers) (smug:parse (timesheet.parser::.date-records) dest)
        (unless (string= leftovers "")
          (cerror "Continue?" 'parsing-error :leftovers leftovers))
        parsed))))

(defun unroll-date (date-obj)
  (with-slots (year month day) date-obj
    (list day month year)))

(defun combine-date-time (time-obj day month year)
  (with-slots (second minute hour) time-obj
    (local-time:encode-timestamp 0 second minute hour
                                 day month year)))

(defun calculate-ranges (ranges year month day)
  (flet ((time-mod-unit-keyword (time-mod)
           (make-keyword
             (string-upcase
               (if (string= (slot-value time-mod 'unit) "mins")
                 "minute"
                 "hour")))))
    (loop with complete = nil
          with partial = nil
          for (start-obj end-obj mod) in ranges
          for start = (combine-date-time start-obj year month day)
          for end = (when end-obj (combine-date-time end-obj year month day))
          for time-mod = (when mod
                           (let ((unit (time-mod-unit-keyword mod))
                                 (amount (slot-value mod 'timesheet.parser:amount)))
                             (funcall #'local-time-duration:duration unit amount)))
          if end do (push (local-time-duration:timestamp-difference end start) complete)
          else do (push start partial)
          when time-mod do (push time-mod complete)
          finally (return (values complete partial)))))

(defun calculate-rounded-ranges (ranges)
  (flet ((calc-duration-in-15mins (duration)
           (let ((duration-in-minutes (local-time-duration:duration-as duration :minute)))
             (coerce (/ (round duration-in-minutes 15) 4)
                     'float))))
    (calc-duration-in-15mins
      (reduce #'local-time-duration:duration+ ranges
        :initial-value (local-time-duration:duration)))))

(defun get-entry-ranges (entry)
  (let ((date (slot-value entry 'date)))
    (with-slots (year month day) date
      (loop for record in (slot-value entry 'records)
            append (with-slots (client memo ranges) record
                     (multiple-value-bind (complete partial) (calculate-ranges ranges day month year)
                       (list*
                         (make-complete-entry date client memo (calculate-rounded-ranges complete))
                         (when partial
                           (list
                             (make-partial-entry date client memo partial))))))))))

(defun get-log (&optional (file *default-time-sheet-file*))
  (block nil
     (let* ((entries (parse-file file)))
       (loop for entry in entries
             for ranges = (get-entry-ranges entry)
             append ranges))))

(defparameter +pprint-log-option-spec+
  '((("client" #\c) :type boolean :optional t :documentation "Sort by client")
    (("reverse" #\r) :type boolean :optional t :documentation "Reverse sort")
    (("status" #\s) :type boolean :optional t
                    :documentation "Print a summary of the hours worked and the prices")
    (("help" #\h) :type boolean :optional t :documentation "show help")))

(defparameter *version* "0:1")
(defun show-version ()
  (format t "timesheet, common-lisp version ~a~%" *version*))

(defun show-help ()
  (show-version)
  (command-line-arguments:show-option-help +pprint-log-option-spec+ :sort-names t))

(defun sort-by-date (results)
  (stable-sort results #'local-time:timestamp<
               :key (alambda (apply #'local-time:encode-timestamp
                                    (append '(0 0 0 0)
                                            (unroll-date (date it)))))))

(defun group-by-client (incompletes)
  (let ((results (make-hash-table :test 'equalp)))
    (loop for incomplete in incompletes
          do (push incomplete (gethash (client incomplete) results)))
    (hash-table-alist results)))

(defgeneric print-entries (entries)
  (:method ((entries list))
   (mapcar #'print-entries entries))
  (:method ((entry partial-entry))
   (format t "~&~4<~>~a, ~a:~%~{~12<~>one starting at ~a~%~}"
           (client entry)
           (memo entry)
           (mapcar
             (alambda (local-time:format-timestring
                        nil it
                        :format '(:year #\/ (:month 2) #\/ (:day 2) #\Space
                                  (:hour 2) #\: (:min 2) #\: (:sec 2))))
             (start-times entry))))
  (:method ((it complete-entry))
   (format t "~&~4a ~10<~:(~a~)~> ~7,2F hrs ~a~%"
           (date it) (client it) (duration it) (memo it))))

(defgeneric record-client (calc client hours)
  (:method ((calc status-calculator) client hours)
    (let ((client (make-keyword (string-upcase client))))
      (incf (gethash client (clients calc) 0)
            hours))))

(defgeneric update (calculator entry)
  (:method ((calculator status-calculator) entry)
   (incf (total-hours calculator) (duration entry)))
  (:method ((calculator status-line) entry)
   (incf (duration calculator) (duration entry))))

(defun update-clients (clients-hash-table entry)
  (with-accessors ((client client)) entry
    (update (ensure-gethash client clients-hash-table
                            (make-instance 'status-line :client client)) 
            entry)))

(defun calculate-results (results &optional (rate *rate*))
  (let ((status-calculator
          (make-instance 'status-calculator
                         :rate rate
                         :client-totals (make-hash-table :test 'equalp))))
    (prog1 status-calculator
      (loop for entry in results
            do (update-clients (client-totals status-calculator) entry)
            do (update status-calculator entry)))))

(defgeneric total-line (calc rate)
  (:method ((calc status-calculator) rate)
   (with-accessors ((total-hours total-hours)) calc
     (format nil "~26<Total~>:~7,2F hours @ ~7,2F $/hr = $~7,2F"
             total-hours rate (* rate total-hours)))))

(defgeneric calculate-cost (calc time)
  (:method ((calc status-calculator) (status-line status-line))
   (* (rate calc) (duration status-line))))

(defun print-status (results)
  (let* ((status-calculator (calculate-results results)))
    (flet ((print-status-line (status-line)
             (format t "~&~:(~26<~a~>~):~7,2F hours @ ~7,2F $/hr = $~7,2F~%"
                     (client status-line)
                     (duration status-line)
                     (rate status-calculator)
                     (calculate-cost status-calculator status-line))))
      (format t "~&~120,1,0,'-<~>~%")
      (loop with client-totals = (client-totals status-calculator)
            for  client in (sort (hash-table-keys client-totals) #'string-lessp)
            for  status-line = (gethash client client-totals)
            do (print-status-line status-line))
      (format t (total-line status-calculator *rate*)))))

(defun pprint-results (results incompletes status)
  (print-entries results)

  (when incompletes
    (format t "~&~120,1,0,'-<~>~%Partial Entries:~%")
    (print-entries incompletes))

  (when status
    (print-status results)))

(defun group-by-class (list &optional accum1 accum2)
  (tagbody ; Let's do some TCO ...
    start
    (if (null list)
      (return-from group-by-class (list accum1 accum2))
      (destructuring-bind (head . tail) list
        (etypecase head
          (complete-entry (setf accum1 (cons head accum1))) ; Here we set the accumulators
          (partial-entry (setf accum2 (cons head accum2)))) ;  to the appropriate values.
        (setf list tail) ; Here we step towards the terminating condition
        (go start))))) ; Recurse

(defun pprint-log (args &key client reverse status help)
  (when help
    (show-help)
    (return-from pprint-log))

  (flet ((sort-results (results &optional (client client))
           (setf results (sort-by-date results))
           (when client
             (setf results (stable-sort results #'string-lessp :key #'client)))
           (when reverse
             (setf results (nreverse results)))
           results))

    (let ((*default-time-sheet-file* (or (car args) *default-time-sheet-file*))
          (*print-pretty* t))
      (destructuring-bind (complete-ranges incomplete-ranges) (group-by-class (get-log *default-time-sheet-file*))
        (let ((complete-results (sort-results complete-ranges))
              (incomplete-results (sort-results incomplete-ranges t)))
          (pprint-results complete-results incomplete-results status))))))

(defun pprint-log-main (argv)
  (setf *rate* (ubiquitous:defaulted-value 0 :rate)
        *default-time-sheet-file* (ubiquitous:defaulted-value #p"~/time.md" :timesheet :file))
  (command-line-arguments:handle-command-line
    +pprint-log-option-spec+
    'pprint-log
    :command-line (cdr argv)
    :name "timesheet"
    :rest-arity t))

