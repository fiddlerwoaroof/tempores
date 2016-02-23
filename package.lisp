;;;; package.lisp
(defpackage #:timesheet.packages
  (:use #:cl))
(in-package #:timesheet.packages)


(defpackage #:generic-equals
  (:use #:cl)
  (:export #:==))

(defpackage #:timesheet.macros
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils #:generic-equals)
  (:export #:make-equality #:make-simple-equality #:defmethod-and-inverse
           #:define-printer #:quick-equalities))


(defpackage #:timesheet.parser
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils #:smug
        #:timesheet.macros #:generic-equals)
  (:shadow #:parse)
  (:export #:parse #:unparse #:date #:records #:client #:ranges #:memo #:hour #:minute #:second
           #:day-of-week #:year #:month #:day #:amount #:unit))

(defpackage #:timesheet.mvc
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils)
  (:export #:model #:view #:controller #:display #:operate #:has-changed))

(defpackage #:timesheet
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils
        #:timesheet.parser))
