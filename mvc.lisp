
(in-package :timesheet.mvc)

(defclass model () ())

(defclass view () ())

(defclass controller () ())

(defclass event () ())

(defgeneric operate (controller model event))
(defgeneric model-changed (model receiver))

(defclass client (model)
  ((name :initarg :name)
   (projects :initarg :projects :initform nil)))

(defun make-client (name)
  (make-instance 'client :name name))

(defun normalize-name (name)
  (string-capitalize name))

(defparameter *clients* nil)
(defun make-or-retrieve-client (name)
  (let ((name (normalize-name name)))
    (cdr (or (assoc name *clients* :test #'string=)
             (car (push (cons name (make-client name)) *clients*))))))

(defclass project-description (model)
  ((client :initarg :client :type 'client)
   (note :initarg :note :initform "")))

(defun make-project-description (client-name note)
  (make-instance 'project-description
                 :client (make-or-retrieve-client client-name)
                 :note note))

(defclass generic-log (model)
  ((entries :initarg :entries :initform nil)))

(defgeneric most-recent-entry (container)
  (:method ((container log-entry))
   (with-slots (entries) container
     (car entries))))

(defclass generic-log-entry (model) ())

(defun %add-entry (container entry)
  (push entry (slot-value container 'entries)))

(defgeneric add-entry (log entry))

(defclass time-log (generic-log)
  ((date :initarg :date :initform (local-time:today))))

(defun make-time-log (&optional date)
  (make-instance 'time-log :date (or date (local-time:today))))

(defclass time-span (model)
  ((start :initarg :start :initform (local-time:now))
   (end :initarg :end)))

(defclass time-log-entry (generic-log)
  ((description :initarg :description :type 'project-description)))

(defmethod add-entry ((time-log-entry time-log-entry) (entry time-span))
  (%add-entry time-log-entry entry))

(defmethod add-entry ((time-log time-log) (entry time-log-entry))
  (%add-entry time-log entry))

(defclass work-log (generic-log)
  ((title :initarg :title :initform "")
   (user :initarg :user :initform "")))

(defmethod add-entry ((work-log work-log) (entry time-log))
  (%add-entry work-log entry))

(defun make-work-log (title user)
  (make-instance 'work-log :title title :user user))


;; VIEW

(defgeneric display (model view output))

(defclass log-view (view) ())

(defmethod display ((model work-log) (view log-view) (output stream))
  (let ((spinneret:*html* output))
    (with-slots (title user entries) model
      (spinneret:with-html
        (:html
          (:head
            (:title (format nil "~a: ~a" user title)))
          (:body
            (:h1 title)
            (:ul.work-log
              (loop for time-log in entries
                    do (with-slots (date entries) time-log
                         (spinneret:with-html
                           (:li
                             (:div.date date)
                             (:ul.time-entries
                               (:li.time-span
                                 (loop for time-entry in entries
                                       do (with-slots (description entries) time-entry
                                            (spinneret:with-html
                                              (:div.description
                                                (loop for time-entry in entries
                                                      do (with-slots (start end) time-entry
                                                           (:div.timespan
                                                             (:span.start (format nil "~a" start))
                                                             (when end
                                                               (:span.end (format nil "~a" end))))))
                                                (with-slots (client note) description
                                                  (spinneret:with-html
                                                    (:span.name (with-slots (name) client
                                                                  (format nil "~a:" name)))
                                                    (:span.note note))))))))))))))))))))

(defmethod display (model view (output (eql nil)))
  (with-output-to-string (s)
    (display model view s)))
