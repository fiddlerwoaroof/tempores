(in-package :asdf-user)
;;;; tempores.asd

(asdf:defsystem #:tempores
  :description "Describe tempores here"
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
               (:file "parser-classes")
               (:file "parser")
               (:file "test-parser")
               (:file "mvc")
               (:file "main-classes")  
               (:file "freshbooks")  
               (:file "tempores")))


;; vim: set ft=lisp:
