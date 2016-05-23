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

;; This will help make sure everything is consumed when
;; we don't care about the parser's output.
(defun cdar-equal (a b) (== (cdar a) (cdar b)))

