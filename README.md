A little utility to manage a simple time-sheet file.

The file-format is this:

```
-- Someday YYYY-MM-DD
   start@HH:MM[:SS][(+|-)NN(mins|hrs)][,HH:MM[:SS][(+|-)NN(mins|hrs)]]
   Client Name: This is a memo.	
```

The parser currently has some rudimentary error handling: it detects invalid indentation (i.e. the lines of
a record do not begin with either a single tab or three spaces) and it detects invalid times.

Additionally, if run in interactive mode `-i`, when it discovers invalid input, it will prompt for a replacement
and attempt to correct the error.  Also, with `-W`, it should be able to recover from whitespace errors on its own.

## Install:

Just do this:

```
CC=`which gcc` \
sbcl --no-userinit \
     --load ~/quicklisp/setup.lisp \
     --eval '(push (truename ".") asdf:*central-registry*)' \
     --eval '(ql:quickload :timesheet)'  \
     --load timesheet-client.lisp \
     --eval '(timesheet.cli::make-executable)'
```

## Todo:

- Expand error handling
- Add alternative export formats
- Add querying capabilities
- Support some notion of sub-tasks

## Examples:

```
Usage: timesheet [-sWircvh] [OPTIONS] TIMESHEETS ...

A program for managing logs of hours worked
Display options
  -s, --status                Print a short summary of work status
  -W, --ignore-whitespace     Ignore whitespace errors in input
  -i, --interactive           Run interactively
Sort options
  -r, --reverse               Reverse the sort direction
  -c, --client                Sort records by client
Freshbooks
  --post-hours                Post hours to freshbooks (requires manual setup of Freshbooks keys)
Self-test options
  --run-tests                 Run the tests
  --output-style=TYPE         The kind of output to produce
                              Default: normal
Generic options
  -v, --version               Show the program version
  -h, --help                  Show this help
```

By default, it orders the log by dates.  With the `-r` option, it displays the dates in descending order:

```
% ./timesheet -r sample-inputs/test.ts
Thu, 2016/01/04  Client #2    5.00 hrs  Implement prototype and write presentation
Thu, 2016/01/04  Client #1    9.00 hrs  Implement Facebook Connector
Wed, 2016/01/03  Client #2    2.50 hrs  Discussed user requirements and produce specification.
Wed, 2016/01/03  Client #1    6.00 hrs  Delivered prototype, reviewed prototype feedback.
Tue, 2016/01/02  Client #1    8.00 hrs  Prototype for testing user experience.
Mon, 2016/01/01  Client #1    8.00 hrs  Mockup of site layout
```

With `-c` it sorts by clients and then does a stable-sort by date:

```
% ./timesheet -sc sample-inputs/test.ts
Mon, 2016/01/01  Client #1    8.00 hrs  Mockup of site layout
Tue, 2016/01/02  Client #1    8.00 hrs  Prototype for testing user experience.
Wed, 2016/01/03  Client #1    6.00 hrs  Delivered prototype, reviewed prototype feedback.
Thu, 2016/01/04  Client #1    9.00 hrs  Implement Facebook Connector
Wed, 2016/01/03  Client #2    2.50 hrs  Discussed user requirements and produce specification.
Thu, 2016/01/04  Client #2    5.00 hrs  Implement prototype and write presentation
------------------------------------------------------------------------------------------------------------------------
                 Client #1:  31.00 hours @   XX.00 $/hr = $XXXX.00
                 Client #2:   7.50 hours @   XX.00 $/hr = $ XXX.00
                     Total:  38.50 hours @   XX.00 $/hr = $XXXX.00
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
                 Client #1:  31.00 hours @   XX.00 $/hr = $XXXX.00
                 Client #2:   7.50 hours @   XX.00 $/hr = $ XXX.00
                     Total:  38.50 hours @   XX.00 $/hr = $XXXX.00
```

`-c` and `-r` combine:

```
% ./timesheet -scr sample-inputs/test.ts
Thu, 2016/01/04  Client #2    5.00 hrs  Implement prototype and write presentation
Wed, 2016/01/03  Client #2    2.50 hrs  Discussed user requirements and produce specification.
Thu, 2016/01/04  Client #1    9.00 hrs  Implement Facebook Connector
Wed, 2016/01/03  Client #1    6.00 hrs  Delivered prototype, reviewed prototype feedback.
Tue, 2016/01/02  Client #1    8.00 hrs  Prototype for testing user experience.
Mon, 2016/01/01  Client #1    8.00 hrs  Mockup of site layout
------------------------------------------------------------------------------------------------------------------------
                 Client #1:  31.00 hours @   XX.00 $/hr = $XXXX.00
                 Client #2:   7.50 hours @   XX.00 $/hr = $ XXX.00
                     Total:  38.50 hours @   XX.00 $/hr = $XXXX.00
```

