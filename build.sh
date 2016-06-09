CC=`which gcc` \
sbcl --no-userinit \
     --load ~/quicklisp/setup.lisp \
     --eval '(push (truename ".") asdf:*central-registry*)' \
     --eval '(ql:quickload :tempores)'  \
     --load tempores-client.lisp \
     --eval '(tempores.cli::make-executable)'
