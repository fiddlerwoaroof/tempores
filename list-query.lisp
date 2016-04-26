
(defpackage #:timesheet.ql
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils
        #:timesheet.macros #:generic-equals))

(in-package #:timesheet.ql)

(defstruct (ql-clause (:type vector))
  clause-type clause-parts)

(defun .name ()
  (.map 'string
        (.is (.or #'alpha-char-p
                  #'digit-char-p))))

(defun .order-specifier ()
  (.or (.string= "desc")
       (.string= "asc")))

(defun .order-list (&optional (separator #\,))
  (.map 'list (.let* ((name (.name))
                      (order (.optional
                               (.and (.is #'whitespacep)
                                     (.order-specifier))))
                      (_ (.optional (.char= separator))))
                (.identity (cons name
                                 (or order "asc"))))))

(defun .name-list (&optional (separator #\,))
  (.map 'list (.let* ((name (.map 'string
                                  (.is (.or #'alpha-char-p
                                            #'digit-char-p))))
                      (_ (.optional (.char= separator))))
                (.identity name))))

(defun .select-clause ()
  (.let* ((_ (.string= "select"))
          (_ (.is #'whitespacep))
          (names (.name-list)))
    (.identity (vector :order-clause names))))

(defun .where-clause ()
  (.let* ((_ (.string= "where"))
          (_ (.is #'whitespacep))
          (names (.name-list)))
    (.identity (vector :order-clause names))))

(defun .order-clause ()
  (.let* ((_ (.string= "order by"))
          (_ (.is #'whitespacep))
          (names (.order-list)))
    (.identity (vector :order-clause names))))

(defun .select-statement ()
  (.let* ((_ (.string= "where"))
          (_ (.is #'whitespacep))
          (names (.name-list)))
    (.identity (vector :order-clause names))))

