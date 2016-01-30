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
               )
  :serial t
  :components ((:file "package")
               (:file "mvc")
               (:file "timesheet")))


;; vim: set ft=lisp:
