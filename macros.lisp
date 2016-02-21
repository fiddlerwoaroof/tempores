(in-package #:timesheet.macros)

(defmacro make-equality (class &body test-defs)
  `(defmethod == ((a ,class) (b ,class))
     (declare (optimize (speed 3)))
     (and ,@(loop for (slot . test) in test-defs
                  with test-val = (or (car test) 'eql)
                  collect `(,test-val (slot-value a ',slot)
                                      (slot-value b ',slot))))))

(defmacro make-simple-equality (class &key (test 'eql) &environment env)
  (let ((class-def (find-class class t env)))
    `(defmethod == ((a ,class) (b ,class))
       (declare (optimize (speed 3)))
       (and ,@(loop for slot in (closer-mop:class-direct-slots class-def)
                    collect (let ((slot (closer-mop:slot-definition-name slot)))
                              `(,test (slot-value a ',slot)
                                      (slot-value b ',slot))))))))

(defmacro defmethod-and-inverse (name (arga argb) &body body)
  `(progn
     (defmethod ,name (,arga ,argb)
       (declare (optimize (speed 3)))
       ,@body)
     (defmethod ,name (,argb ,arga)
       (declare (optimize (speed 3)))
       ,@body)))

(defmacro define-printer ((obj stream &key (type t) (identity t)) (&body pretty) (&body normal))
  `(defmethod print-object ((,obj ,obj) ,stream)
     (if *print-pretty*
       (progn
         ,@pretty)
       (print-unreadable-object (,obj ,stream :type ,type :identity ,identity)
         ,@normal))))

(defmacro quick-equalities (&body defs)                        
  `(progn                                                     
     ,@(loop for (name test)  in defs                         
             collect (list 'make-equality name :test test)))) 

