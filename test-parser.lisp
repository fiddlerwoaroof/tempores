(in-package #:tempores.parser)

;; This will help make sure everything is consumed when
;; we don't care about the parser's output.

(defun cdar-equal (a b) (== (cdar a) (cdar b)))

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

(st:deftest range-list-test ()
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

(st:deftest time-test ()
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

(st:deftest digit-test ()
  (loop for char in '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
        do (st:should be == `((,char . ""))
                      (run (.digit) (make-string 1 :initial-element char)))) )

(st:deftest minute-test ()
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

(st:deftest hour-test ()
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

(st:deftest month-test ()
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
             (run (.time-range) "00:00:00--01:00:00")) 

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

(st:deftest memo-test ()
  (st:should be == '(("asdf" . ""))
             (run (.client-name) "asdf:"))
  (st:should be == '(("asdf" . ""))
             (run (.memo) (format nil " asdf")))
  (st:should be == '((("asdf" "asdf") . ""))
             (run (.memo-line) (format nil "   asdf: asdf"))))

(st:deftest initial-space-test ()
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

(st:deftest make-time-mod-test ()
  (st:should be ==
             (make-instance 'time-mod :unit :hour :amount 0) 
             (make-time-mod 0 "hours"))

  (st:should be ==
             (make-time-mod 0 "hours")
             (make-time-mod 0 "hours"))

  (st:should be ==
             (make-time-mod 0 "hr")
             (make-time-mod 0 "hours"))

  (st:should be ==
             (make-time-mod 0 "hrs")
             (make-time-mod 0 "hours"))

  (st:should be ==
             (make-time-mod 0 "min")
             (make-time-mod 0 "minutes"))

  (st:should be ==
             (make-time-mod 0 "mins")
             (make-time-mod 0 "minutes")))

(st:deftest date-test ()
  (st:should be == nil
             (caar (smug:run (.date) "Monday 2020/01-01")))
  (st:should be == (make-date-obj "Monday" 2020 01 01)
             (caar (smug:run (.date) "Monday, 2020-01-01")))
  (st:should be == (make-date-obj "Monday" 2020 01 01)
             (caar (smug:run (.date) "Monday 2020-01-01")))
  (st:should be == (make-date-obj "Monday" 2020 01 01)
             (caar (smug:run (.date) "Monday 2020/01/01"))))

(st:deftest generic-eq-test ()
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
