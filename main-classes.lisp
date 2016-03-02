(in-package #:timesheet)

(defclass status-calculator ()
  ((rate :initarg :rate :accessor rate)
   (total-hours :initform 0 :initarg :total-hours :accessor total-hours)
   (client-totals :initarg :client-totals :accessor client-totals :initform (make-hash-table :test 'equalp))))

(defun make-status-calculator (rate)
  (make-instance 'status-calculator :rate rate))

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

(defgeneric total-line (calc rate)
  (:method ((calc status-calculator) rate)
   (with-accessors ((total-hours total-hours)) calc
     (format nil "~26<Total~>:~7,2F hours @ ~7,2F $/hr = $~7,2F"
             total-hours rate (* rate total-hours)))))

(defgeneric calculate-cost (calc time)
  (:method ((calc status-calculator) (status-line status-line))
   (* (rate calc) (duration status-line))))

(defgeneric print-entries (entries)
  (:method ((entries list))
   (mapcar #'print-entries entries))
  (:method ((entry partial-entry))
   (format t "~&~4<~>~a, ~a:~%~{~12<~>one starting at ~a~%~}"
           (client entry)
           (memo entry)
           (mapcar
             (alambda (local-time:format-timestring
                        nil it
                        :format '(:year #\/ (:month 2) #\/ (:day 2) #\Space
                                  (:hour 2) #\: (:min 2) #\: (:sec 2))))
             (start-times entry))))
  (:method ((it complete-entry))
   (format t "~&~4a ~10<~:(~a~)~> ~7,2F hrs ~a~%"
           (date it) (client it) (duration it) (memo it))))

(defgeneric record-client (calc client hours)
  (:method ((calc status-calculator) client hours)
    (let ((client (make-keyword (string-upcase client))))
      (incf (gethash client (clients calc) 0)
            hours))))

(defgeneric update (calculator entry)
  (:method ((calculator status-calculator) entry)
   (incf (total-hours calculator) (duration entry)))
  (:method ((calculator status-line) entry)
   (incf (duration calculator) (duration entry))))


(defmethod duration ((obj partial-entry))
  (warn "incomplete entry detected for ~a" (client obj))
  (local-time-duration:duration))

(defun make-complete-entry (date client memo duration)
  (make-instance 'complete-entry
                 :date date
                 :client client
                 :memo memo
                 :duration duration))

(defun make-partial-entry (date client memo start-times)
  (make-instance 'partial-entry
                 :date date
                 :client client
                 :memo memo
                 :start-times start-times))

