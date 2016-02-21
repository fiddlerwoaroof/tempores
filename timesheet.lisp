;;;; timesheet.lisp

(in-package #:timesheet)

(ubiquitous:restore 'timesheet)

;;; "timesheet" goes here. Hacks and glory await!

(defvar *default-time-sheet-file*)
(defvar *rate*)

(defun parse-file (&optional (file *default-time-sheet-file*))
  (with-open-file (s file :direction :input)
    (let ((dest (make-string (file-length s))))
      (read-sequence dest s)
      (caar (smug:run (timesheet.parser::.date-records) dest)))))

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
    (loop for (start-obj end-obj mod) in ranges
          for start = (combine-date-time start-obj year month day)
          for end = (combine-date-time end-obj year month day)
          for time-mod = (when mod
                           (let ((unit (time-mod-unit-keyword mod))
                                 (amount (slot-value mod 'timesheet.parser:amount)))
                             (funcall #'local-time-duration:duration unit amount)))
          nconc (list
                  (local-time-duration:timestamp-difference end start)
                  (or time-mod (local-time-duration:duration))))))

(defun calculate-rounded-ranges (ranges)
  (flet ((calc-duration-in-15mins (duration)
           (let ((duration-in-minutes (local-time-duration:duration-as duration :minute)))
             (coerce (/ (round duration-in-minutes  15) 4)
                     'float))))
    (calc-duration-in-15mins
      (reduce #'local-time-duration:duration+ ranges
        :initial-value (local-time-duration:duration)))))

(defun get-log (&optional (file *default-time-sheet-file*))
  (block nil
         (let* ((entries (parse-file file)))
           (loop for entry in entries
                 for date = (slot-value entry 'date)
                 nconc (with-slots (year month day) date
                         (loop for record in (slot-value entry 'records)
                               collect (with-slots (client memo ranges) record
                                         `(,date
                                            ,client
                                            ,(calculate-rounded-ranges
                                               (calculate-ranges ranges day month year))
                                            ,memo))))))))

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
                                            (unroll-date (car it)))))))

(defun pprint-results (results status)
  (let ((clients (make-hash-table))
        (total-cost 0))

    (flet ((record-client (client hours)
             (let ((client (make-keyword (string-upcase client))))
               (incf (gethash client clients 0) hours))))
      (format t "~&~:{~4a ~10<~:(~a~)~> ~7,2F hrs  ~a~%~}" results)
      (when status
        (format t "~120,1,0,'-<~>")
        (let ((total (format nil "~26<Total~>:~7,2F hours @ ~7,2F $/hr = $~7,2F"
                             (loop for (_ client time __) in results
                                   do (progn _ __)
                                   sum time
                                   do (record-client client time)
                                   do (incf total-cost (* time *rate*)))
                             *rate*
                             total-cost)))
          (flet ((fix-assoc (alist)
                   (mapcar (destructuring-lambda ((client . time))
                             (list client time *rate* (* time *rate*)))
                           alist)))
            (format t "~&~:{~:(~26<~a~>~):~7,2F hours @ ~7,2F $/hr = $~7,2F~%~}"
                    (stable-sort
                      (fix-assoc (hash-table-alist clients))
                      #'string<
                      :key (alambda (car it)))))
          (format t total))))))   

(defun pprint-log (args &key client reverse status help)
  (when help
    (show-help)
    (return-from pprint-log))

  (flet ((sort-results (results)
           (setf results (sort-by-date results))
           (when client
             (setf results (stable-sort results #'string-lessp :key #'cadr)))
           (when reverse
             (setf results (nreverse results)))
           results))

    (let* ((*default-time-sheet-file* (or (car args) *default-time-sheet-file*))
           (*print-pretty* t)
           (results (sort-results (get-log *default-time-sheet-file*))))
      (pprint-results results status))))

(defun pprint-log-main (argv)
  (setf *rate* (ubiquitous:defaulted-value 0 :rate)
        *default-time-sheet-file* (ubiquitous:defaulted-value #p"~/time.md" :timesheet :file))
  (command-line-arguments:handle-command-line
    +pprint-log-option-spec+
    'pprint-log
    :command-line (cdr argv)
    :name "timesheet"
    :rest-arity t))

