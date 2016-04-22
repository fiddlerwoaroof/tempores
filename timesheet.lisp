;; timesheet.lisp

(in-package #:timesheet)

;;; "timesheet" goes here. Hacks and glory await!

(defmacro maybe-list (test &optional val)
  "If both arguments passed, when test is true, return a list containing val
   or, when test is false, return nil.  If one argument passed, when test names
   something that is not a list, return a list containing it, otherwise
   return nil."
  (once-only (test)
    (let ((test (if val test `(not (listp ,test))))
          (val (if val val test)))
      `(when ,test
         (list ,val)))))

(defclass report ()
  ((status-calculator :initarg :status-calculator :accessor status-calculator)
   (status-lines :initform nil :accessor :status-lines)
   (entries :initform nil :accessor :entries)))

(defvar *default-time-sheet-file*)
(defvar *rate*)
(defparameter *interactive* nil)

(defun try-fix-time (failed-time)
  (let ((time-parts (split-sequence #\: failed-time)))
    (destructuring-bind (hours minutes . optional-seconds) time-parts
      (let ((hours (parse-integer hours))
            (minutes (parse-integer minutes))
            (seconds (parse-integer (or (car optional-seconds) "0"))))
        (if (and (< hours 24) (< minutes 60) (< seconds 60))
          (values (format nil "~2,'0d:~2,'0d:~2,'0d" hours minutes seconds) t)
          (values nil nil))))))

(defun parse-file (&optional (file *default-time-sheet-file*) ignore-whitespace-errors)
  (flet ((parse-string (string)
           (handler-bind ((timesheet.parser::invalid-whitespace
                            (lambda (c) c
                              (let ((extra-whitespace (timesheet.parser::failed-chunk c)))
                                (if (or ignore-whitespace-errors
                                        (when *interactive*
                                          (y-or-n-p "Invalid extra whitespace ~s, truncate?" extra-whitespace)))
                                  (smug:replace-invalid extra-whitespace "")
                                  (progn (format t "~&Whitespace errors~%")
                                         (abort))))))
                            (timesheet.parser::invalid-time
                              (lambda (c) c
                                (let ((time (timesheet.parser::failed-chunk c)))
                                  (multiple-value-bind (new-value success) (try-fix-time time)
                                    (when success
                                      (progn (warn 'autocorrect-warning
                                                   :old-value time
                                                   :new-value new-value)
                                             (smug:replace-invalid time new-value)))
                                    (if *interactive*
                                      (progn
                                        (format *query-io* "Invalid time ~a, replacement? " time)
                                        (finish-output *query-io*)
                                        (let ((replacement (read-line)))
                                          (format t "~&Replacing ~s with ~s.~%---~%" time replacement)
                                          (smug:replace-invalid time replacement)))
                                      (progn
                                        (format t "~&Time ~a is invalid.~%" time)
                                        (abort))))))))
             (smug:parse (timesheet.parser::.date-records) string))))
    (multiple-value-bind (parsed leftovers) (parse-string (read-file-into-string file))
      (loop
          (if (or (null leftovers) (string= leftovers ""))
            (return parsed)
            (cerror "Continue?" 'parsing-error :leftovers leftovers)))
      parsed)))

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

(defun calculate-duration-in-15mins (duration)
  (let ((duration-in-minutes (local-time-duration:duration-as duration :minute)))
    (coerce (/ (round duration-in-minutes 15) 4)
            'float)))

(defun calculate-rounded-ranges (ranges)
  (let-each (:be *)
    (local-time-duration:duration)
    (reduce #'local-time-duration:duration+ ranges :initial-value *)
    (calculate-duration-in-15mins *)))

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
                   (maybe-list partial
                               (make-partial-entry date client memo partial))))))))
    (let-each (:be *)
      (slot-value entry 'records)
      (mapcan #'make-entry *))))

(defun get-log (&optional (file *default-time-sheet-file*) ignore-whitespace)
  (let* ((entries (parse-file file ignore-whitespace)))
    (mapcan #'get-entry-ranges entries)))

(defparameter +pprint-log-option-spec+
  '((("client" #\c) :type boolean :optional t :documentation "Sort by client")
    (("reverse" #\r) :type boolean :optional t :documentation "Reverse sort")
    (("ignore-whitespace" #\W) :type boolean :optional t :documentation "Ignore whitespace errors in input")
    (("interactive" #\i) :type boolean :optional t :documentation "Run Interactively")
    (("version" #\v) :type boolean :optional t :documentation "Version")
    (("status" #\s) :type boolean :optional t
                    :documentation "Print a summary of the hours worked and the prices")
    (("help" #\h) :type boolean :optional t :documentation "show help")))

(defparameter *version* "0:4")

(define-message version-message (version)
  (:own-line () "timesheet file parser, version " :str))

(defun show-version ()
  (version-message t *version*))

(defun show-help ()
  (show-version)
  (command-line-arguments:show-option-help +pprint-log-option-spec+ :sort-names t))

(defun sort-by-date (results)
  (stable-sort results #'local-time:timestamp<
               :key (alambda (apply #'local-time:encode-timestamp
                                    (list* 0 0 0 0 (unroll-date (date it)))))))

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

(define-message status-line-format (client duration rate cost)
  (:own-line ()
    (:titlecase () (:rjust (26) :str))
    ": " (:float 7 2) " hours @ " (:float 7 2) " $/hr = $" (:float 7 2)))

(defun print-status (results)
  (let* ((status-calculator (calculate-results results)))
    (labels ((print-status-line (status-line)
               (with-slots (client duration) status-line
                 (status-line-format t client duration
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

(defun pprint-log (args &key client reverse status help version ignore-whitespace interactive)
  (when help
    (show-help)
    (return-from pprint-log))

  (when version
    (show-version)
    (return-from pprint-log))

  (flet ((sort-results (results &optional (client client))
           (setf results (sort-by-date results))
           (when client
             (setf results (stable-sort results #'string-lessp :key #'client)))
           (when reverse
             (setf results (nreverse results)))
           results)
         (get-logs (files)
           (loop for file in (ensure-list files)
                 append (get-log file ignore-whitespace)) ))

    (let ((*default-time-sheet-file* (or args *default-time-sheet-file*))
          (*interactive* interactive)
          (*print-pretty* t))
      (let-each (:be *)
        (get-logs *default-time-sheet-file*)
        (group-by-class *)
        (destructuring-bind (complete-ranges incomplete-ranges) *
          (let ((complete-results (sort-results complete-ranges client))
                (incomplete-results (sort-results incomplete-ranges t)))
            (pprint-results complete-results incomplete-results status)))))))

(defmacro with-timesheet-configuration (() &body body)
  `(progn
     (ubiquitous:restore 'timesheet)
     (let ((*rate* (ubiquitous:defaulted-value 0 :rate))
           (*default-time-sheet-file*
             (ubiquitous:defaulted-value #p"~/time.md" :timesheet :file)))
       ,@body)))

(defun pprint-log-main (argv)
  (with-timesheet-configuration ()
    (command-line-arguments:handle-command-line
      +pprint-log-option-spec+
      'pprint-log
      :command-line (cdr argv)
      :name "timesheet"
      :rest-arity t)))

