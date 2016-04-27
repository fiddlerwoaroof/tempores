(in-package #:timesheet.cli)

(defparameter *interactive* nil)
(defparameter *version* "0:7")

(defun unroll-date (date-obj)
  (with-slots (year month day) date-obj
    (list day month year)))

(defun split-time (time)
  (let ((time-parts (split-sequence #\: time)))
    (destructuring-bind (hours minutes . optional-seconds) time-parts
      (let ((hours (parse-integer hours))
            (minutes (parse-integer minutes))
            (seconds (parse-integer (or (car optional-seconds) "0")))
            (extra (cdr optional-seconds)))
        (values hours minutes seconds extra)))))

(defun try-fix-time (failed-time)
  (handler-case
    (multiple-value-bind (hours minutes seconds extra) (split-time failed-time)
      (if (and (< hours 24) (< minutes 60) (< seconds 60) (null extra))
        (values (format nil "~2,'0d:~2,'0d:~2,'0d" hours minutes seconds) t)
        (values nil nil)))
    (parse-error (c) c (values nil nil))))

(defun call-with-prompt (stream prompt args cb)
  (apply #'format stream prompt args)
  (finish-output *query-io*)
  (funcall cb (read-line *query-io*)))

(defmacro with-prompt ((result-sym stream prompt &rest args) &body body)
  `(call-with-prompt ,stream ,prompt (list ,@args)
                     (lambda (,result-sym)
                       ,@body)))

(defun abort-with-message (stream message &rest args)
  (apply #'format stream message args)
  (abort))

(define-condition parse-time-error (parse-error)
  ((time-string :initarg :time-string :accessor time-string))
  (:report (lambda (condition stream)
             (format stream "Time input did not parse correctly: ~s" (time-string condition)))))

(defun call-with-prompt-for-time (stream prompt args cb)
  (call-with-prompt
    stream prompt args
    (lambda (time-string)
      (multiple-value-bind (hours minutes seconds extra) (split-time time-string)
        (funcall cb hours minutes seconds extra)))))

(defmacro with-prompt-for-time ((result-syms stream prompt &rest args) &body body)
  `(call-with-prompt-for-time ,stream ,prompt (list ,@args)
                              (lambda (,@result-syms)
                                ,@body)))

(define-message format-time (hours minutes &optional (seconds 0))
  (:decimal 2 '0) #\: (:decimal 2 '0) #\: (:decimal 2 '0))

(defun handle-invalid-time (c) c
  (let ((time (timesheet.parser::failed-chunk c)))
    (multiple-value-bind (new-value success) (try-fix-time time)
      (when success
        (progn (warn 'timesheet::autocorrect-warning
                     :old-value time
                     :new-value new-value)
               (smug:replace-invalid time new-value))))
    (if *interactive*
      (loop
        (handler-case
          (with-prompt-for-time ((hours minutes seconds &rest rest)
                                 *query-io* "Invalid time ~a, replacement? " time)
            (declare (ignore rest))
            (let ((replacement (format-time nil hours minutes seconds)))
              (format *query-io* "~&Replacing ~s with ~s.~%---~%" time replacement)
              (smug:replace-invalid time replacement)))
          (parse-error (c) c (format t "~&Invalid entry.~%"))))
      (abort-with-message t "~&Time ~a is invalid.~%" time))))

(defun handle-invalid-whitespace (ignore-whitespace-errors)
  (lambda (c) c
    (let ((extra-whitespace (timesheet.parser::failed-chunk c)))
      (if (or ignore-whitespace-errors
              (when *interactive*
                (y-or-n-p "Invalid extra whitespace ~s, truncate?" extra-whitespace)))
        (smug:replace-invalid extra-whitespace "")
        (abort-with-message t "~&Whitespace errors~%")))))

(defun parse-file (&optional (file *default-time-sheet-file*) ignore-whitespace-errors)
  (flet ((parse-string (string)
           (handler-bind ((timesheet.parser::invalid-whitespace
                            (handle-invalid-whitespace ignore-whitespace-errors))
                          (parse-error #'handle-invalid-time)
                          (timesheet.parser::invalid-time #'handle-invalid-time) )
             (smug:parse (timesheet.parser::.date-records) string))))
    (multiple-value-bind (parsed leftovers) (parse-string (read-file-into-string file))
      (if (or (null leftovers) (string= leftovers ""))
        parsed
        (cerror "Continue?" 'parsing-error :leftovers leftovers)))))

(defun pprint-results (results incompletes status)
  (print-entries results)

  (when incompletes
    (format t "~&~120,1,0,'-<~>~%Partial Entries:~%")
    (print-entries incompletes))

  (when status
    (print-status results)))

(defun sort-by-date (results)
  (stable-sort results #'local-time:timestamp<
               :key (alambda (apply #'local-time:encode-timestamp
                                    (list* 0 0 0 0 (unroll-date (date it)))))))

(defun maybe-nreverse (flag list)
  (if flag
    (nreverse list)
    list))

(define-modify-macro maybe-nreversef (flag)
                     (lambda (place flag)
                       (maybe-nreverse flag place)))

(defun list-without-nulls (&rest items)
  (loop for item in items
        when item collect item))

(defun pprint-log (args &key client reverse status ignore-whitespace interactive)
  (labels ((sort-func (client)
             (apply #'compose
                    (list-without-nulls
                      (when reverse #'nreverse)
                      (when client
                        (plambda (stable-sort :1 #'string-lessp :key #'client)))
                      #'sort-by-date)))
           (sort-results (results &optional (client client))
             (funcall (sort-func client) results))
           (get-logs (files)
             (loop for file in (ensure-list files)
                   append (timesheet:get-log file ignore-whitespace)) ))

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

(defsynopsis (:postfix "TIMESHEETS ...")
  (text :contents "A program for managing logs of hours worked")
  (group (:header "Display options")
         (flag :short-name "s" :long-name "status"
               :description "Print a short summary of work status")
         (flag :short-name "W"
               :long-name "ignore-whitespace"
               :description "Ignore whitespace errors in input")
         (flag :short-name "i" :long-name "interactive"
               :description "Run interactively"))
  (group (:header "Sort options")
         (flag :short-name "r"
               :long-name "reverse"
               :description "Reverse the sort direction")
         (flag :short-name "c"
               :long-name "client"
               :description "Sort records by client"))
  (group (:header "Freshbooks")
         (flag :long-name "post-hours"
               :description "Post hours to freshbooks (requires manual setup of Freshbooks keys)"))
  (group (:header "Self-test options")
         (flag :long-name "run-tests"
               :description "Run the tests")
         (enum :long-name "output-style"
               :description "The kind of output to produce"
               :default-value :normal
               :enum '(:xunit :normal)))
  (group (:header "Generic options")
         (flag :short-name "v" :long-name "version"
               :description "Show the program version")
         (flag :short-name "h" :long-name "help"
               :description "Show this help")))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (define-message version-message (version)
    (:own-line () "timesheet file parser, version " :str)))

(defun show-version ()
  (version-message t *version*))

(defun tests-main (&optional (output-style nil output-style-p))
  (let ((should-test:*verbose* t))
    (ecase output-style
      (:xunit (should-test:test-for-xunit *standard-output* :package :timesheet.parser))
      (:normal (should-test:test :package :timesheet.parser)))))

(defun pprint-log-main ()
  (make-context)
  (tagbody
    start
    (restart-case
      (cond
        ((getopt :long-name "help") (help))
        ((getopt :long-name "version") (show-version))
        ((getopt :long-name "post-hours") (let ((*print-pretty* nil))
                                            (loop for item in (timesheet.freshbooks::post-time-entries-main)
                                                  do (format t "Posted an entry")
                                                  do (plump:serialize item)
                                                  finally (format t "Don't forget to archive time file."))))
        ((getopt :long-name "run-tests") (tests-main (getopt :long-name "output-style")))
        (t (with-timesheet-configuration ()
             (pprint-log
               (remainder)
               :client (getopt :long-name "client")
               :interactive (getopt :long-name "interactive")
               :ignore-whitespace (getopt :long-name "ignore-whitespace")
               :status (getopt :long-name "status")
               :reverse (getopt :long-name "reverse")))))
      (retry () (go start))
      (abort ()))))

(defun make-executable ()
  (dump "timesheet" pprint-log-main
        :compression 8
        :purify t))
