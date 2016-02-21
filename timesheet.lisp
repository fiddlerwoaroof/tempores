;;;; timesheet.lisp

(in-package #:timesheet)

(ubiquitous:restore 'timesheet)

;;; "timesheet" goes here. Hacks and glory await!

(defvar *default-time-sheet-file*)
(defvar *rate*)

(defun parse-file (&optional (file *default-time-sheet-file*))
  (with-open-file (s *default-time-sheet-file* :direction :input)
    (let ((dest (make-string (file-length s))))
      (read-sequence dest s)
      (caar (smug:run (timesheet.parser::.date-records) dest)))))

(defun unroll-date (date-obj)
  (with-slots (year month day) date-obj
    (list day month year)))

(defun calculate-ranges (ranges year month day)
  (loop for (start-obj end-obj mod) in ranges
        for start = (local-time:encode-timestamp 0 
                                                 (slot-value start-obj 'second)
                                                 (slot-value start-obj 'minute)
                                                 (slot-value start-obj 'hour)
                                                 day month year)
        for end = (local-time:encode-timestamp 0
                                               (slot-value end-obj 'second)
                                               (slot-value end-obj 'minute)
                                               (slot-value end-obj 'hour)
                                               day month year )
        for time-mod = (when time-mod
                         (let ((unit (make-keyword
                                       (string-upcase
                                         (if (string= (slot-value time-mod 'timesheet.parser::unit) "mins")
                                           "minute"
                                           "hour"))))
                               (amount (slot-value time-mod 'timesheet.parser:amount)))
                           (funcall #'local-time-duration:duration unit amount)))
        nconc (list (local-time-duration:timestamp-difference end start)
                    (or time-mod (local-time-duration:duration)))))

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
                                               (calculate-ranges ranges year month day))
                                            ,memo))))))))

(defparameter +pprint-log-option-spec+
  '((("client" #\c) :type boolean :optional t :documentation "sort by client")
    (("reverse" #\r) :type boolean :optional t :documentation "reverse")  
    (("status" #\s) :type boolean :optional t :documentation "status")))

(defparameter *version* "0:1")
(defun show-version ()
  (format t "timesheet, common-lisp version ~a~%" *version*))

(defun show-help ()
  (show-version)
  (command-line-arguments:show-option-help +pprint-log-option-spec+ :sort-names t))

(defun pprint-log (args &key client reverse status help)
  (when help
    (show-help)
    (return-from pprint-log))

  (let* ((*default-time-sheet-file* (or (cadr args) *default-time-sheet-file*))
         (*print-pretty* t)
         (results (get-log *default-time-sheet-file*))
         (clients (make-hash-table))
         (total-cost 0))
    (setf results (stable-sort results #'local-time:timestamp<
                               :key (alambda (apply #'local-time:encode-timestamp
                                                    (append '(0 0 0 0)
                                                            (unroll-date (car it)))))))
    (when client
      (setf results (stable-sort results #'string-lessp :key #'cadr)))
    (when reverse
      (setf results (nreverse results)))
    (format t "~&~:{~4a ~10<~:(~a~)~> ~7,2F hrs  ~a~%~}" results)
    (flet ((record-client (client hours)
             (let ((client (make-keyword (string-upcase client))))
               (incf (gethash client clients 0) hours))))
      (when status
        (format t "~120,1,0,'-<~>")
        (let ((total (format nil "~26<Total~>:~7,2F hours @ ~7,2F $/hr = $~7,2F"
                             (loop for (_ client time ___) in results
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

(defun pprint-log-main (argv)
  (setf *default-time-sheet-file* (ubiquitous:defaulted-value "" :timesheet :file))
  (setf *rate* (ubiquitous:defaulted-value 40 :rate))
  (command-line-arguments:handle-command-line
    +pprint-log-option-spec+
    'pprint-log
    :command-line (cdr argv)
    :name "timesheet"
    :rest-arity t))
