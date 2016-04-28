(in-package #:tempores.parser)

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

; TODO: consider adding one-digit hours
(defun .hour ()
  (.let* ((f (.first-hour-char))
          (s (.digit)))
    (if (or (char/= f #\2) (member s '(#\0 #\1 #\2 #\3))) ;; make sure we don't get 24
      (.identity (coerce (vector f s) 'string))
      (.fail))))

(defun .minute-or-second ()
  (.let* ((f (.first-minute-char))
          (s (.digit)))
    (.identity (coerce (vector f s) 'string))))

(defun .time-range-separator ()
  (.string= "--"))

(define-condition invalid-time (parsing-error) ()
  (:report (lambda (condition stream)
             (format stream "Not a valid time part ~s"
                     (failed-chunk condition)))))

(defun .valid-time ()
  (.let* ((hour (.hour))
          (_ (.time-separator))
          (minute (.minute-or-second))
          (second (.optional
                    (.progn (.time-separator)
                            (.minute-or-second)))))
    (.identity (list hour minute (if second second "0")))))

(defun .invalid-time ()
  (flet ((.non-time-character ()
           (.map 'string
                 (.and (.not (.or (.time-separator)
                                  (.char= #\newline)
                                  (.char= #\space)))
                       (.item)))))
    (.let* ((hour (.or (.hour)
                       (.non-time-character)))
            (_ (.time-separator))
            (minute (.or (.minute-or-second)
                         (.non-time-character)))
            (second (.optional
                      (.progn (.time-separator)
                              (.or (.minute-or-second)
                                   (.non-time-character))))))
      (error 'invalid-time
        :failed-chunk (concatenate 'string
                                   (string hour) ":"
                                   (string minute)
                                   (if second
                                     (concatenate 'string ":" (string second))
                                     ""))))))

(defun .time ()
  (.let* ((time (.or (.valid-time) (.invalid-time))))
    (.identity
      (apply #'make-time-obj
             (mapcar #'parse-integer
                     time)))))

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

(defun .whitespace-char ()
  (.or (.char= #\tab) (.char= #\space)))

(defun .whitespace ()
  (.map 'string (.whitespace-char)))

(defun .valid-initial-space ()
  (.or (.string= (string #\tab))
       (.string= "   ")))

(defun .extra-whitespace ()
  (.let* ((_ (.valid-initial-space))
          (extra-space (.optional (.whitespace))))
    (if extra-space
      (error 'invalid-whitespace :failed-chunk extra-space)
      (.fail))))  

(defun .initial-space ()
  (.or (.extra-whitespace)
       (.valid-initial-space)))

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
  (.or (.char= #\-)
       (.char= #\/)))

(defun .date ()
  (.let* ((dow (.weekday))
          (_ (.optional (.char= #\,)))
          (_ (.char= #\Space))
          (year (.year))
          (sep1 (.date-separator))
          (month (.month))
          (sep2 (.date-separator))
          (day (.day)))
    (let ((year (parse-integer year))
          (month (parse-integer month))
          (day (parse-integer day)))
      (.identity (make-date-obj dow year month day)))))

(st:deftest date-test ()

  (st:should be == nil
             (caar (smug:run (.date) "Monday 2020/01-01")))


  (st:should be == (make-date-obj "Monday" 2020 01 01)
             (caar (smug:run (.date) "Monday, 2020-01-01")))


  (st:should be == (make-date-obj "Monday" 2020 01 01)
             (caar (smug:run (.date) "Monday 2020-01-01")))


  (st:should be == (make-date-obj "Monday" 2020 01 01)
             (caar (smug:run (.date) "Monday 2020/01/01")))
  )

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

#|(defun parse (data)|#
#|  (alet (run (.date-records) data)|#
#|    (values (caar it) (cdar it))))|#

;; This will help make sure everything is consumed when
;; we don't care about the parser's output.
(defun cdar-equal (a b) (== (cdar a) (cdar b)))

(st:deftest initial-space ()
  (st:should signal invalid-whitespace
             (smug:parse (.initial-space) "    "))

  (st:should signal invalid-whitespace
             (smug:parse (.initial-space) (concatenate 'string
                                                       (string #\tab)
                                                       " ")))

  (st:should signal invalid-whitespace
             (smug:parse (.initial-space) (concatenate 'string
                                                       (string #\tab)
                                                       (string #\tab))))

  (st:should signal invalid-whitespace
             (smug:parse (.initial-space) (concatenate 'string
                                                       (string #\tab)
                                                       "      ")))

  (st:should be == (string #\tab)
             (smug:parse (.initial-space) (string #\tab)))

  (st:should be == "   "
             (smug:parse (.initial-space) "   ")))

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

  (st:should signal invalid-time
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
  (st:should signal invalid-time
             (run (.time) "00:0a:00"))

  (st:should be == '(((0 0 0) . ""))
             (handler-bind ((invalid-time
                              (lambda (x) x
                                (smug:replace-invalid "00:0a:00" "00:00:00"))))
               (run (.time) "00:0a:00")))

  (st:should be == '((#\: . ""))
             (run (.time-separator) ":"))

  (st:should signal invalid-time
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

  (st:should be == `(("01" . ""))
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

  (st:should be == `(("20" . ""))
             (run (.prog1 (.hour) (.not (.item))) "20"))

  (st:should be == `(("01" . ""))
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

(st:deftest generic-eq ()
  "Note: this really should be in the equality package with the name ==
   should-test only checks tests for _internal_ symbols."
  (st:should be eql t (== #\1 #\1))
  (st:should be eql t (== 1 1))
  (st:should be eql t (== "1" "1"))
  (st:should be eql t (== '("1") '("1")))
  (st:should be eql t (== #("1") #("1")))
  (st:should be eql t (== '(1 . 2) '(1 . 2)))
  (st:should be eql t (== '((1 . 2)) '((1 . 2))))
  (st:should be eql t (== #1=(make-date-obj "Monday" 2020 01 01) #1#))

  (st:should be eql t
             (== (make-date-obj "Monday" 2012 01 01)
                 (make-date-obj "Monday" 2012 01 01)))

  (st:should be eql t
             (== (make-time-obj 00 00 00)
                 (make-time-obj 00 00 00)))

  (st:should be eql t
             (== (make-time-mod 3 "mins")
                 (make-time-mod 3 "mins")))
  (st:should be eql t
             (== (list (make-time-mod 3 "mins"))
                 (list (make-time-mod 3 "mins"))))
  (st:should be eql t
             (== #((make-time-mod 3 "mins"))
                 #((make-time-mod 3 "mins")))))

