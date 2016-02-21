(in-package :asdf-user)
;;;; timesheet.asd

(asdf:defsystem #:timesheet
  :description "Describe timesheet here"
  :author "fiddlerwoaroof"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum
               #:anaphora
               #:ningle
               #:spinneret
               #:should-test
               #:fwoar.lisputils
               #:smug
               #:cells
               #:ubiquitous
               #:command-line-arguments
               #:manardb
               #:local-time-duration
               )
  :serial t
  :components ((:file "package")
               (:file "generic-equals")
               (:file "macros")
               (:file "parser")
               (:file "mvc")
               (:file "timesheet")))


;; vim: set ft=lisp:
