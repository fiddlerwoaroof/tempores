
(in-package #:timesheet.cli)

(defsynopsis (:postfix "TIMESHEETS ...")
  (text :contents "A program for managing logs of hours worked")
  (group (:header "Main actions")
         (flag :short-name "c"
               :long-name "client"
               :description "Sort records by client")
         (flag :short-name "r"
               :long-name "reverse"
               :description "Reverse the sort direction")
         (flag :short-name "W"
               :long-name "ignore-whitespace"
               :description "Ignore whitespace errors in input")
         (flag :short-name "i" :long-name "interactive"
               :description "Run interactively") 
         (flag :short-name "s" :long-name "status"
               :description "Print a short summary of work status"))
  (group (:header "Other options")
         (flag :short-name "v" :long-name "version"
               :description "Show the program version")  
         (flag :short-name "h" :long-name "help"
               :description "Show this help")))

(defun pprint-log-main ()
  (make-context)
  (cond 
    ((getopt :long-name "help") (help))
    (t (timesheet::with-timesheet-configuration ()
         (timesheet::pprint-log
           (remainder)
           :client (getopt :long-name "client")
           :interactive (getopt :long-name "interactive")  
           :ignore-whitespace (getopt :long-name "ignore-whitespace")  
           :version (getopt :long-name "version")  
           :status (getopt :long-name "status")  
           :reverse (getopt :long-name "reverse"))))))

(defun make-executable ()
  (dump "timesheet" pprint-log-main))
