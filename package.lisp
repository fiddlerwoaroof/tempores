;;;; package.lisp

(defpackage #:timesheet
  (:use #:cl))

(defpackage #:timesheet.mvc
  (:use #:cl #:anaphora #:alexandria #:serapeum)
  (:export #:model #:view #:controller #:display #:operate #:has-changed))
