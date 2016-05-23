;;;; package.lisp
(defpackage #:tempores.packages
  (:use #:cl)
  (:export #:*tempores-packages*))
(in-package #:tempores.packages)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *tempores-packages* '()))

(defpackage #:generic-equals
  (:use #:cl)
  (:export #:==))

(defpackage #:tempores.macros
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils #:generic-equals)
  (:export #:make-equality #:make-simple-equality #:defmethod-and-inverse
           #:define-printer #:quick-equalities))
(push :tempores.macros *tempores-packages*)

(defpackage #:tempores.parser
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils #:smug
        #:tempores.macros #:generic-equals)
  (:shadow #:parse)
  (:export #:parse #:unparse #:date #:records #:client #:ranges #:memo #:hour #:minute #:second
           #:day-of-week #:year #:month #:day #:amount #:unit))
(push :tempores.parser *tempores-packages*)

(defpackage #:tempores.mvc
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils)
  (:export #:model #:view #:controller #:display #:operate #:has-changed))
(push :tempores.mvc *tempores-packages*)

(defpackage #:tempores
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils
        #:tempores.parser)
  (:import-from #:format-string-builder #:define-message)
  (:export #:with-tempores-configuration #:pprint-log #:get-log #:tempores
           #:*default-time-sheet-file* #:*rate* #:group-by-class #:print-status
           #:print-entries #:autocorrect-warning))
(push :tempores *tempores-packages*)

(defpackage #:tempores.cli
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils
        #:tempores.parser #:tempores #:net.didierverna.clon
        #:plambda)
  (:import-from #:format-string-builder #:define-message))
(push :tempores.cli *tempores-packages*)

(in-package #:tempores)

(defvar *default-time-sheet-file*)
(defvar *rate*)
