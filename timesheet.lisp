;; timesheet.lisp

(in-package #:timesheet)

(ubiquitous:restore 'timesheet)

;;; "timesheet" goes here. Hacks and glory await!

(defclass report ()
  ((status-calculator :initarg :status-calculator :accessor status-calculator)
   (status-lines :initform nil :accessor :status-lines)
   (entries :initform nil :accessor :entries)))

(defvar *default-time-sheet-file*)
(defvar *rate*)

(defun parse-file (&optional (file *default-time-sheet-file*))
  (with-open-file (s file :direction :input)
    (let ((dest (make-string (file-length s))))
      (read-sequence dest s)
      (multiple-value-bind (parsed leftovers) (smug:parse (timesheet.parser::.date-records) dest)
        (unless (or (null leftovers) (string= leftovers ""))
          (cerror "Continue?" 'parsing-error :leftovers leftovers))
        parsed))))

(defun unroll-date (date-obj)
  (with-slots (year month day) date-obj
    (list day month year)))

(defun combine-date-time (time-obj day month year)
  (declare (optimize (debug 3)))
  (with-slots (second minute hour) time-obj
    (local-time:encode-timestamp 0 second minute hour
                                 day month year)))

(defun calculate-ranges (ranges date)
  (declare (optimize (debug 3)))
  (labels ((time-mod-unit-keyword (time-mod)
             (make-keyword
               (string-upcase
                 (if (string= (slot-value time-mod 'unit) "mins")
                   "minute"
                   "hour"))))
           (make-mod (mod)
             (when mod
               (let ((unit (time-mod-unit-keyword mod))
                     (amount (slot-value mod 'timesheet.parser:amount)))
                 (funcall #'local-time-duration:duration unit amount)))))
    (with-slots (year month day) date
      (loop with complete = nil
            with partial = nil
            for (start-obj end-obj mod) in ranges
            for start = (combine-date-time start-obj day month year)
            for end = (when end-obj (combine-date-time end-obj day month year))
            for time-mod = (when mod (make-mod mod))
            if end do (push (local-time-duration:timestamp-difference end start) complete)
            else do (push start partial)
            when time-mod do (push time-mod complete)
            finally (return (values complete partial))))))

(defun calculate-rounded-ranges (ranges)
  (flet ((calc-duration-in-15mins (duration)
           (let ((duration-in-minutes (local-time-duration:duration-as duration :minute)))
             (coerce (/ (round duration-in-minutes 15) 4)
                     'float))))
    (calc-duration-in-15mins
      (reduce #'local-time-duration:duration+ ranges
        :initial-value (local-time-duration:duration)))))

(defmacro list-or-null (test val)
  `(when ,test
     (list ,val)))

(defclass log-entry ()
  ((complete :initarg :complete)
   (incomplete :initarg :incomplete)))

(defun get-entry-ranges (entry)
  (flet ((make-entry (record)
           (let ((date (slot-value entry 'date)))
             (with-slots (client memo ranges) record
               (multiple-value-bind (complete partial) (calculate-ranges ranges date)
                 (list*
                   (make-complete-entry date client memo (calculate-rounded-ranges complete))
                   (list-or-null partial
                                 (make-partial-entry date client memo partial))))))))
    (let-each (:be *)
      (slot-value entry 'records)
      (mapcan #'make-entry *))))

(defun get-log (&optional (file *default-time-sheet-file*))
  (let* ((entries (parse-file file)))
    (mapcan #'get-entry-ranges entries)))

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
          for client = (client incomplete)
          do (push incomplete (gethash client results)))
    (hash-table-alist results)))

(defun update-clients (status-calculator entry)
  (flet ((ensure-client (client)
           (ensure-gethash client 
                           (client-totals status-calculator)
                           (make-instance 'status-line :client client))))
    (with-accessors ((client client)) entry
      (let ((client-hash-table (ensure-client client)))
        (update client-hash-table entry)))))

(defun calculate-results (results &optional (rate *rate*))
  (let-first (:be status-calculator) (make-status-calculator rate)
    (dolist (result results)
      (update-clients status-calculator result)
      (update status-calculator result))))

;;   Uses the first arg as a list. Adds 26 blanks to left
(defparameter +status-line-format-string+ "~&~:@{~:(~26<~a~>~):~7,2F hours @ ~7,2F $/hr = $~7,2F~}~%") 
(defun print-status (results)
  (let* ((status-calculator (calculate-results results)))
    (labels ((status-line-format (&rest args)
               (format t +status-line-format-string+ args))
             (print-status-line (status-line)
               (with-slots (client duration) status-line
                 (status-line-format
                   client
                   duration
                   (rate status-calculator)
                   (calculate-cost status-calculator status-line))))
             (print-separator ()
               (format t "~&~120,1,0,'-<~>~%")))
      (let ((client-totals (client-totals status-calculator)))
        (print-separator)
        (let-each (:be *)
          (hash-table-keys client-totals)
          (sort * #'string-lessp)
          (dolist (client *)
            (print-status-line (gethash client client-totals)))))
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
      (let-each (:be *)
        (get-log *default-time-sheet-file*)
        (group-by-class *)
        (destructuring-bind (complete-ranges incomplete-ranges) *
          (let ((complete-results (sort-results complete-ranges client))
                (incomplete-results (sort-results incomplete-ranges t)))
            (pprint-results complete-results incomplete-results status)))))))

(defun pprint-log-main (argv)
  (setf *rate* (ubiquitous:defaulted-value 0 :rate)
        *default-time-sheet-file* (ubiquitous:defaulted-value #p"~/time.md" :timesheet :file))
  (command-line-arguments:handle-command-line
    +pprint-log-option-spec+
    'pprint-log
    :command-line (cdr argv)
    :name "timesheet"
    :rest-arity t))

