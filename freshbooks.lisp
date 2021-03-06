(defpackage #:tempores.freshbooks
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils
        #:tempores.parser)
  (:import-from #:tempores #:tempores)
  (:export #:tempores))

(in-package :tempores.freshbooks)

(defvar *api-key*)
(defvar *endpoint*)

(defun init ()
  (ubiquitous:restore 'tempores)
  (ubiquitous:value :freshbooks :api-key)
  (ubiquitous:value :freshbooks :endpoint))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (xhtmlambda::def-element <::request) 
  (xhtmlambda::def-element <::time_entry)
  (xhtmlambda::def-element <::project_id)
  (xhtmlambda::def-element <::task_id)
  (xhtmlambda::def-element <::hours)
  (xhtmlambda::def-element <::notes)
  (xhtmlambda::def-element <::date))

(defun post-to-endpoint (xml)
  (let ((drakma:*text-content-types* (acons "application" "xml" drakma:*text-content-types*)))
    (drakma:http-request *endpoint*
                         :basic-authorization (list *api-key* "X")
                         :method :post
                         :content (with-output-to-string (s)
                                    (format s "~w" xml)))))

(defun parsed-api-call (xml)
  (plump:parse (post-to-endpoint xml)))

(defmacro define-api-call (name (&rest args) method-name &body elements)
  `(defun ,name ,args
     (parsed-api-call (<:request (:method ,method-name) ,@elements))))

(define-api-call create-time-entry (entry-spec) "time_entry.create"
  entry-spec)

(define-api-call list-invoices () "invoice.list")
(define-api-call list-projects () "project.list")
(define-api-call list-tasks () "task.list")
(define-api-call list-payments () "payment.list")
(define-api-call list-clients () "client.list")

(defgeneric slots-for (cls)
  (:method-combination append))

(defmacro define-simple-class (name (&rest supers) &body elements)
  (let ((schema-name (intern
                       (string-join (list name "-SCHEMA"))))
        (schema-supers (loop for super in supers
                             collect (intern
                                       (string-join
                                         (list (symbol-name super)
                                               "-schema"))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (prog1
         (defclass ,name ,supers
           ,(list*
              '(registry :initform (make-hash-table :test 'equal) :allocation :class)
              (loop for element in elements
                    collect `(,element :initarg ,(make-keyword element) :initform nil))))
         (defclass ,schema-name ,schema-supers ())
         (defmethod slots-for append ((cls ,schema-name))
           ',elements)))))

(define-simple-class task ()
  task_id name description billable rate)

(define-simple-class project ()
  project_id name description rate bill_method client_id hour_budget
  tasks staff)

(tempores.macros:define-printer (task s)
  ((with-slots (task_id name) task
    (format s "~i~a (~a):" name task_id)))
  ((with-slots (task_id name) task
    (format s "~a (~a)" name task_id))))

(tempores.macros:define-printer (project s)
  ((with-slots (project_id name tasks) project
    (format s "~i~a (~a):~%~{~a~%~}" name project_id tasks)))
  ((with-slots (project_id name tasks) project
    (format s "~a (~a): ~a tasks" name project_id (length tasks)))))

(defparameter *task-registry* (make-hash-table :test 'equal))
(defmethod initialize-instance :after ((self task) &key &allow-other-keys)
  (loop for slot in '(task_id name description billable rate)
        for node = (slot-value self slot)
        unless (null node) do (setf (slot-value self slot) (plump:text node))))

(defun register-task (task)
  (with-slots (task_id) task
    (setf (gethash task_id *task-registry*) task)))

(defparameter *project-registry* (make-hash-table :test 'equal))
(defmethod initialize-instance :after ((self project) &key &allow-other-keys)
  (loop for slot in '(project_id name description rate bill_method client_id hour_budget)
        for node = (slot-value self slot)
        unless (null node) do (setf (slot-value self slot) (plump:text node)))

  (with-slots (tasks) self
    (setf tasks
          (loop for task across (lquery:$ (inline tasks)  "> task")
                for task_id = (lquery:$1 (inline task) "task_id" (text))
                for task-obj = (gethash task_id *task-registry*)
                unless task-obj do (setf task-obj (parse-task task))
                collect task-obj))))

(defun register-project (project)
  (with-slots (name) project
    (setf (gethash name *project-registry*) project)))



(defun parse-task (parsed-xml)
  (apply #'make-instance 'task
         (loop for slot in (slots-for (make-instance 'task-schema))
               nconc (let ((slot-name (format nil "> ~(~a~)" slot)))
                       (list (make-keyword slot)
                             (lquery:$ (inline parsed-xml)
                                       slot-name
                                       (node)))))))

(defun parse-project (parsed-xml)
  (apply #'make-instance 'project
         (loop for slot in (slots-for (make-instance 'project-schema))
               nconc (let ((slot-name (format nil "> ~(~a~)" slot)))
                       (list (make-keyword slot)
                             (lquery:$ (inline parsed-xml)
                                       slot-name
                                       (node)))))))

(defun get-project (name)
  (let ((projects (sort (map 'vector
                             (alambda (cons (string-downcase it) it))
                             (hash-table-keys *project-registry*))
                        #'string<
                        :key #'car))
        (name (string-downcase name)))
    (gethash
      (cdr
        (find-if (alambda (string= it name :end1 (length name)))
                 projects
                 :key #'car))
      *project-registry*)))


(defun make-time-entry (project task date hours notes)
  (<:time_entry ()
                (<:project_id ()
                              (parse-integer
                                (slot-value (get-project project)
                                            'project_id)))
                (<:task_id () (parse-integer task))
                (<:date () (identity date))
                (<:hours () (identity hours))
                (<:notes () (identity notes))))

(defun get-task-by-name (name)
  (let ((tasks (mapcar
                 (destructuring-lambda ((id . task))
                   (cons (slot-value task 'name)
                         id))
                 (hash-table-alist *task-registry*))))
    (cdr (assoc name tasks :test #'string-equal))))

(defun tempores-to-entries (tempores-log)
  (let ((task-id (get-task-by-name "General")))
    (loop for entry in tempores-log
          for date = (tempores::date entry)
          for project = (tempores::client entry)
          for note = (tempores::memo entry)
          for hours = (tempores::duration entry)
          for fmt-date = (format nil "~:@{~2,'0d-~2,'0d-~2,'0d 00:00:00~}"
                                 (reverse (tempores.cli::unroll-date date)))
          collect (make-time-entry project task-id fmt-date hours note))))

(defun make-entry-updates ()
  (let ((updates (tempores-to-entries (tempores::get-log #p"/home/edwlan/bucket/time.md"))))
    (loop for update in updates
          collect (<:request (:method "time_entry.create") update))))

(defun get-entry-data ()
  (init)
  (setf *endpoint* (ubiquitous:value :freshbooks :endpoint))
  (setf *api-key* (ubiquitous:value :freshbooks :api-key))

  (let-each (:be *)
    (list-tasks)
    (lquery:$ * "task")
    (map 'list #'parse-task *)
    (mapcar #'register-task *))

  (let-each (:be *)
    (list-projects)
    (lquery:$ * "project")
    (map 'list #'parse-project *)
    (mapcar #'register-project *))

  (make-entry-updates))

(defun post-time-entries-main ()
  (let-each (:be *)
    (get-entry-data)
    (mapcar #'parsed-api-call *)))
