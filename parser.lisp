(in-package #:timesheet.parser)

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
     (unit :initarg :unit))))

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
(make-simple-equality date-obj :test eql)
(make-simple-equality time-mod :test equal)

 
(defun make-day-entry (date records)
  (make-instance 'day-entry :date date :records records))

(defun make-time-record (ranges memo)
  (make-instance 'time-record :client (car memo) :ranges ranges :memo (cadr memo)))

(defun make-time-mod (amnt unt)
  (setf unt (string-downcase unt))
  (when (string= "min" unt)
    (setf unt "mins"))
  (when (string= "hr" unt)
    (setf unt "hours"))
  (alet (make-instance 'time-mod)
    (with-slots (amount unit) it
      (setf amount amnt unit unt)
      it)))

(defun make-date-obj (day-of-week year month day)
  (make-instance 'date-obj :day-of-week day-of-week :year year :month month :day day))

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
  ()
  ((with-slots (hour minute second) time-obj
    (format s "~2,'0d:~2,'0d:~2,'0d"  hour minute second))))

(define-printer (day-entry s)
  ()
  ((with-slots (date records) day-entry
    (format s "~d records for ~s" (length records) date))))

(define-printer (time-record s)
  ()
  ((with-slots (client) time-record
    (format s "For ~s" client))))

(define-printer (time-mod s)
  ()
  ((with-slots (amount unit) time-mod
    (format s "~s ~s" amount unit))))

(defun .digit ()
  (.is #'digit-char-p))

(defun .first-hour-char ()
  (.is (lambda (x)
         (member x '(#\0 #\1 #\2)))))

(defun .first-minute-char ()
  (.is (lambda (x)
         (member x '(#\0 #\1 #\2 #\3 #\4 #\5)))))

(defun .time-separator ()
  (.char= #\:))

(defun .hour ()
  (.let* ((f (.first-hour-char))
          (s (.digit)))
    (if (or (char/= f #\2) (member s '(#\0 #\1 #\2 #\3))) ;; make sure we don't get 24
      (.identity (parse-integer (coerce (vector f s) 'string)))
      (.fail))))

(defun .minute-or-second ()
  (.let* ((f (.first-minute-char))
          (s (.digit)))
    (.identity (parse-integer (coerce (vector f s) 'string)))))

(defun .time-range-separator ()
  (.string= "--"))

(defun .time ()
  (.let* ((hour (.hour))
          (_ (.time-separator))
          (minute (.minute-or-second))
          #|(_ (.time-separator))|#
          (second (.optional
                    (.progn (.time-separator)
                            (.minute-or-second)))))
    (.identity (make-time-obj hour minute (or second 0)))))

(defun .time-unit ()
  (.or
    (.string= "mins")  
    (.string= "hrs")
    (.string= "min")
    (.string= "hr")))

(defun .time-mod ()
  (.let* ((sign (.or (.char= #\+) (.char= #\-)))
          (num (.first (.map 'string (.digit))))
          (unit (.time-unit)))
    (.identity
      (make-time-mod
        (parse-integer
          (with-output-to-string (s)
            (princ sign s)
            (princ num s)))
        unit))))

(defun .peek (parser)
  (lambda (input)
    (if (run parser input)
      (list (cons t input))
      (run (.fail) input))))

(defun .time-range ()
  (.let* ((start (.time))
          (_ (.prog1 
               (.time-range-separator)
               (.peek (.not (.char= #\,)))))
          (done (.optional (.time)))
          (mod (.optional (.time-mod))))
    (when (and mod (not done))
      (.fail))
    (if done
      (if mod
        (.identity
          (list start done mod))
        (.identity (list start done)))
      (.identity (list start)))))

(defun .zero-or-more (parser)
  (.plus (.let* ((x parser)
                 (xs (.zero-or-more parser)))
           (.identity (cons x xs)))
         (.identity ())))

(defun .range-list-separator ()
  (.char= #\,))

(defun .range-list ()
  (.let*
    ((ranges (.prog1
               (.map 'list
                     (.prog1 (.time-range)
                             (.optional (.progn (.range-list-separator)
                                                (.zero-or-more (.char= #\Space))))))
               (.char= #\Newline))))
    (let ((lengths (map 'vector #'length ranges)))
      (if (/= 0 (elt lengths (1- (length lengths))))
        (.identity ranges)
        (.fail)))))

(defun .initial-space ()
  (.string= "   "))

(defun .time-line-start ()
  (.progn (.initial-space)
          (.string= "start@")))

(defun .time-line ()
  (.progn
    (.time-line-start)
    (.range-list)))

(defun .client-separator ()
  (.char= #\:))

(defun .client-name ()
  (.prog1
    (.map 'string (.and (.not (.client-separator)) (.item)) :at-least 0)
    (.client-separator)))

(defun .memo ()
  (.prog2
    (.map 'string (.char= #\Space))
    (.first
      (.map 'string
          (.and (.not (.char= #\Newline))
                (.item))))
    (.optional (.not (.item)))))

(defun .memo-line ()
  (.progn
    (.initial-space)
    (.let* ((client (.client-name))
            (memo (.memo)))
      (.identity (list client memo)))))

(defun .record ()
  (.let* ((time-line (.time-line))
          (memo-line (.memo-line)))
    (.identity (make-time-record time-line memo-line))))

(defun .records ()
  (.first
    (.map 'list (.prog1 (.record)
                        (.or (.map 'string (.char= #\Newline))
                             (.progn
                               (.char= #\Newline)
                               (.not (.item)))
                             (.not (.item)))))))

(defun .weekday ()
  (.or (.string-equal "Sunday")
       (.string-equal "Monday")
       (.string-equal "Tuesday")
       (.string-equal "Wednesday")
       (.string-equal "Thursday")
       (.string-equal "Friday")
       (.string-equal "Saturday")))

(defun .year ()
  (.let* ((fi (.digit))
          (se (.digit))
          (th (.digit))
          (fo (.digit)))
    (.identity (coerce (list fi se th fo) 'string))))

(defun .first-month-char ()
  (.or (.char= #\0) (.char= #\1) (.char= #\2) (.char= #\3)))

(defun .first-day-char ()
  (.or (.char= #\0) (.char= #\1) (.char= #\2) (.char= #\3)))

(defun .month ()
  (.let* ((fi (.first-month-char))
          (se (.digit)))
    (let ((res (when (char= fi #\3)
                 (unless (member se '(#\1 #\0))
                   (.fail)))))
      (if (not res)
        (.identity (coerce (list fi se) 'string))
        res))))

(defun .day ()
  (.let* ((fi (.first-month-char))
          (se (.digit)))
    (when (and (char= fi #\3) (not (member se '(#\0 #\1))))
      (.fail))
    (.identity (coerce (list fi se) 'string))) )

(defun .date-separator ()
  (.char= #\-))

(defun .date ()
  (.let* ((dow (.weekday))
          (_ (.char= #\Space))
          (year (.year))
          (_ (.date-separator))
          (month (.month))
          (_ (.date-separator))
          (day (.day)))
    (let ((year (parse-integer year))
          (month (parse-integer month))
          (day (parse-integer day)))
      (.identity (make-date-obj dow year month day)))))

(defun .date-start ()
  (.string= "-- "))

(defun .date-line ()
  (.prog2 (.date-start)
          (.date)
          (.char= #\Newline)))

(defun .date-record ()
  (.let* ((date (.date-line))
         (records (.records)))
    (.identity (make-day-entry date records))))

(defun .date-records ()
  (.first (.map 'list (.date-record))))

(defun .parse-all-records ()
  (.prog1 (.date-records) (.not (.item))))

(defun parse (data)
  (alet (run (.date-records) data)
    (values (caar it) (cdar it))))

;; This will help make sure everything is consumed when
;; we don't care about the parser's output.
(defun cdar-equal (a b) (== (cdar a) (cdar b)))

(st:deftest memo-test ()
  (st:should be == '(("asdf" . ""))
             (run (.client-name) "asdf:"))

  (st:should be == '(("asdf" . ""))
             (run (.memo) (format nil " asdf")))

  (st:should be == '((("asdf" "asdf") . ""))
             (run (.memo-line) (format nil "   asdf: asdf"))))

(st:deftest time-line-test ()
  (st:should be cdar-equal '(("   start@" . ""))
             (run (.time-line-start) "   start@"))

  (st:should be == '(((((0 0 0))) . ""))
             (run (.time-line) (format nil "   start@00:00:00--~%")))

  (st:should be == '(((((0 0 0) (1 0 0))) . ""))
             (run (.time-line) (format nil "   start@00:00:00--01:00:00~%")))

  (st:should be == '(((((0 0 0) (1 0 0))
                          ((2 0 0)))
                         . ""))
             (run (.time-line) (format nil "   start@00:00:00--01:00:00,02:00:00--~%")))

  (st:should be == '(((((0 0 0) (1 0 0))
                          ((2 0 0) (3 0 0)))
                         . ""))
             (run (.time-line) (format nil "   start@00:00:00--01:00:00,02:00:00--03:00:00~%")))

  (st:should be == '(((((0 0 0) (1 0 0))
                          ((2 0 0)))
                         . ""))
             (run (.time-line) (format nil "   start@00:00:00--01:00:00, 02:00:00--~%"))))

(should-test:deftest range-list-test ()
  (st:should be == '((#\, . ""))
             (run (.range-list-separator) ","))

  (st:should be == nil
             (run (.range-list) "30:00:00"))

  (st:should be == nil
             (run (.range-list) "00:00:00"))

  (st:should be == nil
             (run (.range-list) "00:00:00--,00:00:00"))

  (st:should be == '(((((0 0 0))) . ""))
             (run (.range-list) (format nil "00:00:00--~%")))

  (st:should be == '(((((0 0 0) (1 0 0))) . ""))
             (run (.range-list) (format nil "00:00:00--01:00:00~%")))

  (st:should be == '(((((0 0 0) (1 0 0))
                          ((2 0 0)))
                         . ""))
             (run (.range-list) (format nil "00:00:00--01:00:00,02:00:00--~%")))

  (st:should be == '(((((0 0 0) (1 0 0))
                          ((2 0 0) (3 0 0)))
                         . ""))
             (run (.range-list) (format nil "00:00:00--01:00:00,02:00:00--03:00:00~%")))

  (st:should be == `(((((0 0 0) (1 0 0) ,(make-time-mod -10 "mins")) 
                       ((2 0 0) (3 0 0))
                       )
                      . ""))
             (run (.range-list) (format nil "00:00:00--01:00:00-10mins,02:00:00--03:00:00~%")))

  (st:should be == `(((((0 0 0) (1 0 0) ,(make-time-mod 10 "mins"))  
                       ((2 0 0) (3 0 0))
                       )
                      . ""))
             (run (.range-list) (format nil "00:00:00--01:00:00+10mins,02:00:00--03:00:00~%")))

  (st:should be == '(((((0 0 0) (1 0 0))
                          ((2 0 0)))
                         . ""))
             (run (.range-list) (format nil "00:00:00--01:00:00, 02:00:00--~%")))) ;; space allowed between ranges

(should-test:deftest time-range-test ()
  (st:should be == '(("--" . ""))
             (run (.time-range-separator) "--"))

  (st:should be == nil
       (run (.time-range) "30:00:00"))

  (st:should be == nil
       (run (.time-range) "00:00:00"))

  (st:should be == nil
       (run (.time-range) "00:00:00--,01:00:00--"))

  (st:should be == '((((0 0 0)) . ""))
       (run (.time-range) "00:00:00--"))

  (st:should be == '((((0 0 0) (1 0 0)) . ""))
       (run (.time-range) "00:00:00--01:00:00")))

(should-test:deftest time-test ()
  (st:should be == '((#\: . ""))
             (run (.time-separator) ":"))

  (st:should be == nil
       (run (.time) "30:00:00"))

  (st:should be == '(((0 0 0) . ""))
       (run (.time) "00:00:00")))

(should-test:deftest digit-test ()
  (loop for char in '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
        do (st:should be == `((,char . ""))
                      (run (.digit) (make-string 1 :initial-element char)))) )

(should-test:deftest minute-test ()
  (st:should be == nil
             (run (.first-minute-char) "a"))
  (st:should be == nil
             (run (.first-minute-char) "6"))
  (st:should be == nil
             (run (.first-minute-char) "-1"))
  (loop for char in '(#\0 #\1 #\2 #\3 #\4 #\5)
        do (st:should be == `((,char . ""))
                      (run (.first-minute-char) (make-string 1 :initial-element char))))

  (st:should be == nil
             (run (.minute-or-second) "61"))

  (st:should be == nil
             (run (.minute-or-second) "71"))

  (st:should be == nil
             (run (.minute-or-second) "0"))  ;; one digit

  (st:should be == nil
             (run (.minute-or-second) "aa"))

  (st:should be == `((1 . ""))
             (run (.minute-or-second) "01")))


(should-test:deftest hour-test ()
  (st:should be == nil
             (run (.first-hour-char) "a"))
  (st:should be == nil
             (run (.first-hour-char) "3"))
  (st:should be == nil
             (run (.first-hour-char) "-1"))

  (st:should be eq T
          (every #'identity
                 (loop for char in '(#\0 #\1 #\2)
                       collect (== `((,char . ""))
                                   (run (.first-hour-char) (make-string 1 :initial-element char))))))

  (st:should be == nil
             (run (.hour) "24"))

  (st:should be == nil
             (run (.hour) "71"))

  (st:should be == nil
             (run (.hour) "0"))

  (st:should be == nil
             (run (.hour) "aa"))

  (st:should be == `((20 . ""))
             (run (.prog1 (.hour) (.not (.item))) "20")) 

  (st:should be == `((1 . ""))
             (run (.prog1 (.hour) (.not (.item))) "01")))

(should-test:deftest month-test ()
  (st:should be == nil
             (run (.first-month-char) "a"))
  (st:should be == nil
             (run (.first-month-char) "4"))
  (st:should be == nil
             (run (.first-month-char) "-1"))

  (loop for char in '(#\0 #\1 #\2 #\3)
        do (st:should be == `((,char . ""))
                      (run (.first-month-char) (make-string 1 :initial-element char))))

  (st:should be == nil
             (run (.month) "32"))

  (st:should be == nil
             (run (.month) "71"))

  (st:should be == nil
             (run (.month) "0"))

  (st:should be == nil
             (run (.month) "aa"))

  (st:should be == `(("30" . ""))
             (run (.prog1 (.month) (.not (.item))) "30")) 
  (st:should be == `(("20" . ""))
             (run (.prog1 (.month) (.not (.item))) "20")) 
  (st:should be == `(("10" . ""))
             (run (.prog1 (.month) (.not (.item))) "10")) 
  (st:should be == `(("01" . ""))
             (run (.prog1 (.month) (.not (.item))) "01")))

(st:deftest time-range-test ()
  
  (st:should be == nil
             (run (.time-range) "00:00:00"))

  (st:should be == `(( (,(make-time-obj 0 0 0)) . ""))
             (run (.time-range) "00:00:00--"))

  (st:should be == `(( (,(make-time-obj 0 0 0)) . ""))
             (run (.time-range) "00:00--"))

  (st:should be == `(((,(make-time-obj 0 0 0) ,(make-time-obj 1 0 0)) . ""))
             (run (.time-range) "00:00:00--01:00:00"))

  (st:should be == `(((,(make-time-obj 0 0 0) ,(make-time-obj 1 0 0)) . ""))
             (run (.time-range) "00:00--01:00"))

  (st:should be == `(((,(make-time-obj 0 0 0) ,(make-time-obj 1 0 0) ,(make-time-mod 10 "mins")) . ""))
             (run (.time-range) "00:00--01:00+10mins"))

  (st:should be == `(((,(make-time-obj 0 0 0) ,(make-time-obj 1 0 0) ,(make-time-mod -10 "mins")) . ""))
             (run (.time-range) "00:00--01:00-10mins")))

(st:deftest == ()
  (st:should be eql t (== #\1 #\1))
  (st:should be eql t (== 1 1))
  (st:should be eql t (== "1" "1"))
  (st:should be eql t (== '("1") '("1")))
  (st:should be eql t (== #("1") #("1")))
  (st:should be eql t (== '(1 . 2) '(1 . 2)))
  (st:should be eql t (== '((1 . 2)) '((1 . 2))))
  (st:should be eql t
             (== (make-time-mod 3 "mins")
                 (make-time-mod 3 "mins"))) 
  (st:should be eql t
             (== (list (make-time-mod 3 "mins"))
                 (list (make-time-mod 3 "mins"))))
  (st:should be eql t
             (== #((make-time-mod 3 "mins"))
                 #((make-time-mod 3 "mins")))))

