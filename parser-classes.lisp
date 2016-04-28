(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; make sure these classes am ready to go!
  (defclass day-entry ()
    ((date :initarg :date)
     (records :initarg :records)))

  (defclass time-record ()
    ((client :initarg :client)
     (ranges :initarg :ranges)
     (memo :initarg :memo)))

  (defclass time-obj ()
    ((hour :initarg :hour)
     (minute :initarg :minute)
     (second :initarg :second)))

  (defclass date-obj ()
    ((day-of-week :initarg :day-of-week)
     (year :initarg :year)
     (month :initarg :month)
     (day :initarg :day)))

  (defclass time-mod ()
    ((amount :initarg :amount)
     (unit :initarg :unit :type (member '(:hour :minute))))))

(defgeneric unparse (token &optional stream)
  (:method ((token time-mod) &optional stream)
   (with-slots (amount unit) token
     (format stream "~@d~a" amount unit)))
  (:method ((token date-obj)  &optional stream)
   (with-slots (day-of-week year month day) token
     (format stream "~a ~2,'0d-~2,'0d-~2,'0d" day-of-week year month day)))
  (:method ((token time-obj) &optional stream)
   (with-slots (hour minute second) token
     (format stream "~2,'0d:~2,'0d:~2,'0d" hour minute second)))
  (:method ((token time-record) &optional stream)
   (with-slots (client ranges memo) token
     (format stream "~&   start@~{~a~^,~}~%   ~a: ~a~%"
             (loop for (start . rest) in ranges
                   for end = (car rest)
                   for mod = (cadr rest)
                   collect (format nil "~a--~a~a"
                                   (unparse start)
                                   (if end (unparse end) "")
                                   (if mod (unparse mod) "")))
             client memo)))
  (:method ((token day-entry) &optional stream)
   (with-slots (date records) token
     (format stream "~&-- ~a~&~{~a~}~%"
             (unparse date)
             (mapcar #'unparse records)))))

(make-simple-equality day-entry :test ==)
(make-simple-equality time-record :test ==)
(make-simple-equality time-obj :test eql)
(make-equality date-obj
  (day-of-week ==)
  (year) (month) (day))
(make-simple-equality time-mod :test eql)

(defun make-day-entry (date records)
  (make-instance 'day-entry :date date :records records))

(defun make-time-record (ranges memo)
  (make-instance 'time-record :client (car memo) :ranges ranges :memo (cadr memo)))

(defun make-time-mod (amnt unt)
  (setf unt (string-downcase unt))
  (make-instance 'time-mod
                 :amount amnt
                 :unit (string-ecase unt
                         ("min" :minute)
                         ("mins" :minute)
                         ("minutes" :minute)
                         ("minute" :minute)
                         ("hr" :hour)
                         ("hrs" :hour)
                         ("hour" :hour)
                         ("hours" :hour))))

(define-condition parsing-error (parse-error)
  ((failed-chunk :initarg :failed-chunk :reader failed-chunk)))

(define-condition invalid-whitespace (parsing-error) ()
  (:report (lambda (condition stream)
             (format stream "~s is invalid whitespace"
                     (map 'list #'char-code (failed-chunk condition))))))

(define-condition invalid-day-of-week (parsing-error)
  ((day-of-week :initarg :day-of-week :reader day-of-week))
  (:report (lambda (condition stream)
             (format stream "~s is not a valid day of the week"
                     (day-of-week condition)))))

(defun make-date-obj (day-of-week year month day)
  (let ((day-of-week (subseq day-of-week 0 3)))
    (if (member day-of-week
                '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
                :test #'string-equal)
      (make-instance 'date-obj :day-of-week day-of-week :year year :month month :day day)
      (error 'invalid-day-of-week :day-of-week day-of-week))))

(defun make-time-obj (hour minute &optional second)
  (make-instance 'time-obj :hour hour :minute minute :second second))

(defmethod-and-inverse == ((date-obj date-obj) (list list))
  (with-slots (day-of-week year month day) date-obj
    (every #'== (list day-of-week year month day) list)))

(defmethod-and-inverse == ((time-obj time-obj) (list list))
  (with-slots (hour minute second) time-obj
    (every #'== (list hour minute second) list)))

(define-printer (date-obj s)
  ((with-slots (day-of-week year month day) date-obj
     (format s "~a, ~2,'0d/~2,'0d/~2,'0d"
             (subseq (string-capitalize day-of-week) 0 3)
             year month day)))
  ((with-slots (day-of-week year month day) date-obj
    (format s "~a, ~2,'0d/~2,'0d/~2,'0d"
            (subseq (string-capitalize day-of-week) 0 3)
            year month day))))

(define-printer (time-obj s)
  ((with-slots (hour minute second) time-obj
    (format s "~2,'0d:~2,'0d:~2,'0d"  hour minute second)))
  ((with-slots (hour minute second) time-obj
    (format s "~2,'0d:~2,'0d:~2,'0d"  hour minute second))))

(define-printer (day-entry s)
  ((with-slots (date records) day-entry
    (format s "~d records for ~s" (length records) date)))
  ((with-slots (date records) day-entry
    (format s "~d records for ~s" (length records) date))))

(define-printer (time-record s)
  ((with-slots (client) time-record
    (format s "For ~s" client)))
  ((with-slots (client) time-record
    (format s "For ~s" client))))

(define-printer (time-mod s)
  ((with-slots (amount unit) time-mod
    (format s "~s ~s" amount unit)))
  ((with-slots (amount unit) time-mod
    (format s "~s ~s" amount unit))))

