;;;; package.lisp

(defpackage #:timesheet.parser
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils #:smug)
  )

(defpackage #:timesheet
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils)
  )

(defpackage #:timesheet.mvc
  (:use #:cl #:anaphora #:alexandria #:serapeum #:fwoar.lisputils)
  (:export #:model #:view #:controller #:display #:operate #:has-changed))
