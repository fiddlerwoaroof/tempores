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
        #:timesheet.parser)
  (:import-from #:format-string-builder #:define-message)
  (:export #:with-timesheet-configuration #:pprint-log #:get-log #:timesheet
           #:*default-time-sheet-file* #:*rate* #:group-by-class #:print-status
           #:print-entries #:autocorrect-warning))

(defpackage #:timesheet.cli
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils
        #:timesheet.parser #:timesheet #:net.didierverna.clon
        #:plambda)
  (:import-from #:format-string-builder #:define-message))

(in-package #:timesheet)

(defvar *default-time-sheet-file*)
(defvar *rate*)
