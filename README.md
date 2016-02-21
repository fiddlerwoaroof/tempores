A little utility to manage a simple time-sheet file.

The file-format is this:

```
-- Someday YYYY-MM-DD
   start@HH:MM[:SS][+-NN(mins|hrs)][,HH:MM[:SS][+-NN(mins|hrs)]]
   Client Name: This is a memo.	
```

Write now the parser is fairly fragile: it reds what it can and fails silently at the first 
error.  Eventually there'll be better error-handling.

```
% ./timesheet -h
timesheet, common-lisp version 0:1
  -c --client                     boolean  Sort by client
  -r --reverse                    boolean  Reverse sort
  -s --status                     boolean  Print a summary of the hours worked and the prices
  -h --help                       boolean  show help
```

```
% ./timesheet -r sample-inputs/test.ts
Thu, 2016/01/04  Client #2    5.00 hrs  Implement prototype and write presentation
Thu, 2016/01/04  Client #1    9.00 hrs  Implement Facebook Connector
Wed, 2016/01/03  Client #2    2.50 hrs  Discussed user requirements and produce specification.
Wed, 2016/01/03  Client #1    6.00 hrs  Delivered prototype, reviewed prototype feedback.
Tue, 2016/01/02  Client #1    8.00 hrs  Prototype for testing user experience.
Mon, 2016/01/01  Client #1    8.00 hrs  Mockup of site layout
```

```
% ./timesheet -sc sample-inputs/test.ts
Mon, 2016/01/01  Client #1    8.00 hrs  Mockup of site layout
Tue, 2016/01/02  Client #1    8.00 hrs  Prototype for testing user experience.
Wed, 2016/01/03  Client #1    6.00 hrs  Delivered prototype, reviewed prototype feedback.
Thu, 2016/01/04  Client #1    9.00 hrs  Implement Facebook Connector
Wed, 2016/01/03  Client #2    2.50 hrs  Discussed user requirements and produce specification.
Thu, 2016/01/04  Client #2    5.00 hrs  Implement prototype and write presentation
------------------------------------------------------------------------------------------------------------------------
                 Client #1:  31.00 hours @   40.00 $/hr = $1240.00
                 Client #2:   7.50 hours @   40.00 $/hr = $ 300.00
                     Total:  38.50 hours @   40.00 $/hr = $1540.00
```

```
% ./timesheet -sr sample-inputs/test.ts
Thu, 2016/01/04  Client #2    5.00 hrs  Implement prototype and write presentation
Thu, 2016/01/04  Client #1    9.00 hrs  Implement Facebook Connector
Wed, 2016/01/03  Client #2    2.50 hrs  Discussed user requirements and produce specification.
Wed, 2016/01/03  Client #1    6.00 hrs  Delivered prototype, reviewed prototype feedback.
Tue, 2016/01/02  Client #1    8.00 hrs  Prototype for testing user experience.
Mon, 2016/01/01  Client #1    8.00 hrs  Mockup of site layout
------------------------------------------------------------------------------------------------------------------------
                 Client #1:  31.00 hours @   40.00 $/hr = $1240.00
                 Client #2:   7.50 hours @   40.00 $/hr = $ 300.00
                     Total:  38.50 hours @   40.00 $/hr = $1540.00
```

```
% ./timesheet -scr sample-inputs/test.ts
Thu, 2016/01/04  Client #2    5.00 hrs  Implement prototype and write presentation
Wed, 2016/01/03  Client #2    2.50 hrs  Discussed user requirements and produce specification.
Thu, 2016/01/04  Client #1    9.00 hrs  Implement Facebook Connector
Wed, 2016/01/03  Client #1    6.00 hrs  Delivered prototype, reviewed prototype feedback.
Tue, 2016/01/02  Client #1    8.00 hrs  Prototype for testing user experience.
Mon, 2016/01/01  Client #1    8.00 hrs  Mockup of site layout
------------------------------------------------------------------------------------------------------------------------
                 Client #1:  31.00 hours @   40.00 $/hr = $1240.00
                 Client #2:   7.50 hours @   40.00 $/hr = $ 300.00
                     Total:  38.50 hours @   40.00 $/hr = $1540.00
```
