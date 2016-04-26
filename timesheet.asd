(in-package :asdf-user)
;;;; timesheet.asd

(asdf:defsystem #:timesheet
  :description "Describe timesheet here"
  :author "fiddlerwoaroof"
  :license "MIT"
  :depends-on (#:alexandria
               #:anaphora
               #:cells
               #:net.didierverna.clon
               #:command-line-arguments
               #:drakma
               #:format-string-builder
               #:fwoar.lisputils
               #:local-time-duration
               #:lquery
               #:ningle
               #:positional-lambda
               #:serapeum
               #:should-test
               #:smug
               #:spinneret
               #:ubiquitous
               #:xhtmlambda
               )
  :serial t
  :components ((:file "package")
               (:file "generic-equals")
               (:file "macros")
               (:file "parser")
               (:file "mvc")
               (:file "main-classes")  
               (:file "freshbooks")  
               (:file "timesheet")))


;; vim: set ft=lisp:
