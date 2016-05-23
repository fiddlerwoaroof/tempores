(defpackage #:tempores.package-grapher
  (:use #:cl))

(in-package #:tempores.package-grapher)

(defun get-dependencies (package)
  (let ((source-package-name package)
        (package (find-package package)))
    (remove-duplicates
      (stable-sort 
        (loop for x being the symbols in package
              unless (eq package (symbol-package x))
              collect (list source-package-name
                            (alexandria:make-keyword (package-name (symbol-package x)))))
        #'string< 
        :key #'cadr)
      :test 'equal)))

(defun get-all-dependencies (packages)
  (stable-sort 
    (loop for package in packages
          nconc (get-dependencies package)) 
    #'string< 
    :key #'cadr))

(defun make-graph-conns (conns)
  (format nil "~:{~4t~(\"~a\" -> \"~a\"~%~)~}" conns))

(defun make-graph (conns)
  (format nil "digraph {~%~a~%}~%" (make-graph-conns conns)))

(defun graph-packages (packages)
  (make-graph (get-all-dependencies packages)))

(defun graph-tempores-packages ()
  (graph-packages tempores.packages:*tempores-packages*))
