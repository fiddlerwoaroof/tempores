(in-package #:timesheet)

(defclass status-calculator ()
  ((rate :initarg :rate :accessor rate)
   (total-hours :initform 0 :initarg :total-hour :accessor total-hours)
   (client-totals :initarg :client-totals :accessor client-totals)))

(defclass status-line ()
  ((client :initarg :client :accessor client)
   (duration :initarg :duration :accessor duration :initform 0)))

(defclass parsed-entry ()
  ((date :initarg :date :accessor date)
   (client :initarg :client :accessor client)
   (memo :initarg :memo :accessor memo)))

(defclass complete-entry (parsed-entry)
  ((duration :initarg :duration :accessor duration)))

(defclass partial-entry (parsed-entry)
  ((start-times :initarg :start-times :initform nil :accessor start-times)))

(define-condition incomplete-entry-warning (warning) ())

(define-condition parsing-error ()
  ((leftovers :initarg :leftovers :accessor leftovers))
  (:report (lambda (condition stream)
               (format stream "Parse error: ~20s leftover" (leftovers condition)))))


