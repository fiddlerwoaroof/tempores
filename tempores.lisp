;; tempores.lisp

(in-package #:tempores)

;;; "tempores" goes here. Hacks and glory await!

(defmacro maybe-list (test &optional val)
  "If both arguments passed, when test is true, return a list containing val
   or, when test is false, return nil.  If one argument passed, when test names
   something that is not a list, return a list containing it, otherwise
   return nil."
  (once-only (test)
    (let ((test (if val test `(not (listp ,test))))
          (val (if val val test)))
      `(when ,test
         (list ,val)))))

(defun combine-date-time (time-obj day month year)
  (declare (optimize (debug 3)))
  (with-slots (second minute hour) time-obj
    (local-time:encode-timestamp 0 second minute hour
                                 day month year)))

(defun calculate-ranges (ranges date)
  (declare (optimize (debug 3)))
  (labels ((time-mod-unit-keyword (time-mod)
             (make-keyword
               (string-upcase
                 (string-case (string-downcase (slot-value time-mod 'unit))
                   ("mins" "minute")
                   (t      "hour")))))
           (make-mod (mod)
             (when mod
               (let ((unit (time-mod-unit-keyword mod))
                     (amount (slot-value mod 'tempores.parser:amount)))
                 (funcall #'local-time-duration:duration unit amount)))))
    (with-slots (year month day) date
      (loop with complete = nil
        with partial = nil
        for (start-obj end-obj mod) in ranges
        for start = (combine-date-time start-obj day month year)
        for end = (when end-obj (combine-date-time end-obj day month year))
        for time-mod = (when mod (make-mod mod))
        if end do (push (local-time-duration:timestamp-difference end start) complete)
        else do (push start partial)
        when time-mod do (push time-mod complete)
        finally (return (values complete partial))))))

(defun calculate-duration-in-15mins (duration)
  (let ((duration-in-minutes (local-time-duration:duration-as duration :minute)))
    (coerce (/ (round duration-in-minutes 15) 4)
            'float)))

(defun calculate-rounded-ranges (ranges)
  (let-each (:be *)
    (local-time-duration:duration)
    (reduce #'local-time-duration:duration+ ranges :initial-value *)
    (calculate-duration-in-15mins *)))

(defclass log-entry ()
  ((complete :initarg :complete)
   (incomplete :initarg :incomplete)))

(defun get-entry-ranges (entry)
  (flet ((make-entry (record)
           (let ((date (slot-value entry 'date)))
             (with-slots (client memo ranges) record
               (multiple-value-bind (complete partial) (calculate-ranges ranges date)
                 (list*
                   (make-complete-entry date client memo (calculate-rounded-ranges complete))
                   (maybe-list partial
                               (make-partial-entry date client memo partial))))))))
    (let-each (:be *)
      (slot-value entry 'records)
      (mapcan #'make-entry *))))

(defun get-log (&optional (file *default-time-sheet-file*) ignore-whitespace)
  (let* ((entries (tempores.cli::parse-file file ignore-whitespace)))
    (mapcan #'get-entry-ranges entries)))

(defun group-by-client (incompletes)
  (let ((results (make-hash-table :test 'equalp)))
    (loop for incomplete in incompletes
          for client = (client incomplete)
          do (push incomplete (gethash client results)))
    (hash-table-alist results)))

(defun update-clients (status-calculator entry)
  (flet ((ensure-client (client)
           (ensure-gethash client
                           (client-totals status-calculator)
                           (make-instance 'status-line :client client))))
    (with-accessors ((client client)) entry
      (let ((client-hash-table (ensure-client client)))
        (update client-hash-table entry)))))

(defun calculate-results (results &optional (rate *rate*))
  (let-first (:be status-calculator) (make-status-calculator rate)
    (dolist (result results)
      (update-clients status-calculator result)
      (update status-calculator result))))

(define-message status-line-format (client duration rate cost)
  (:own-line ()
   (:titlecase () (:rjust (26) :str))
   ": " (:float 7 2) " hours @ " (:float 7 2) " $/hr = $" (:float 7 2)))

(defun print-status (results)
  (let* ((status-calculator (calculate-results results))
         (client-totals (client-totals status-calculator)))
    (labels ((print-status-line (status-line)
               (with-slots (client duration) status-line
                 (status-line-format t client duration
                                     (rate status-calculator)
                                     (calculate-cost status-calculator status-line))))
             (print-separator ()
               (format t "~&~120,1,0,'-<~>~%")))
      (let-each (:be *)
        (print-separator)
        (hash-table-keys client-totals)
        (sort * #'string-lessp)
        (dolist (client *)
          (print-status-line (gethash client client-totals)))
        (format t (total-line status-calculator *rate*))))))


(defun group-by-class (list &optional accum1 accum2)
  (tagbody ; Let's do some TCO ...
    start
    (if (null list)
      (return-from group-by-class (list accum1 accum2))
      (destructuring-bind (head . tail) list
        (etypecase head
          (complete-entry (setf accum1 (cons head accum1))) ; Here we set the accumulators
          (partial-entry (setf accum2 (cons head accum2)))) ;  to the appropriate values.
        (setf list tail) ; Here we step towards the terminating condition
        (go start))))) ; Recurse

(defmacro with-tempores-configuration (() &body body)
  `(progn
     (ubiquitous:restore 'tempores)
     (let ((*rate* (ubiquitous:defaulted-value 0 :rate))
           (*default-time-sheet-file*
             (ubiquitous:defaulted-value #p"~/bucket/time.md" :tempores :file)))
       ,@body)))

