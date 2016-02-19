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
               #:manardb)
  :serial t
  :components ((:file "package")
               (:file "parser")
               (:file "mvc")
               (:file "timesheet")))


;; vim: set ft=lisp:
