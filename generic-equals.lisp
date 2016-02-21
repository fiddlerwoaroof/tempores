(in-package #:generic-equals)

(defgeneric == (a b)
  (:method (a b) (eql a b))
  (:method ((a list) (b list))
   (declare (optimize (speed 3) (space 3)))
   (if (or (null a) (null b))
     (and (null a) (null b))
     (and (== (car a) (car b))
          (== (cdr a) (cdr b)))))
  (:method ((a vector) (b vector))
   (declare (optimize (speed 3) (space 3)))
   (every #'identity (map 'vector #'== a b))))


