(defpackage :maxclaims/database
  (:use :common-lisp)
  (:import-from :cl-postgres
		#:database-error
		#:database-error-code
		#:database-error-cause
		#:database-error-detail)
  (:export 
	   #:maxclaims-unique-violation
	   #:maxclaims-database-condition))

(in-package :maxclaims/database)

(define-condition maxclaims-unique-violation (database-error)
  ((attribute :initarg :attribute
	      :reader maxclaims-unique-violation-attribute)
   (value :initarg :value
	  :reader maxclaims-unique-violation-value))
  (:report (lambda (c s)
	     (format s "~A : ~A is not unique"
		     (maxclaims-unique-violation-attribute c)
		     (maxclaims-unique-violation-value c)))))
  

(defun maxclaims-database-condition (condition)
  "http://www.postgresql.org/docs/current/static/errcodes-appendix.html

23505	unique_violation"
  
  (cond 
    ((string= (database-error-code condition) "23505")
     ;; unique_violation
     (make-condition 'maxclaims-unique-violation
		     :attribute (subseq (database-error-detail condition)
					(1+ (position #\( (database-error-detail condition)))
					(position #\) (database-error-detail condition)))
		     :value (let ((val (subseq (database-error-detail condition)
					(+ 2 (position #\= (database-error-detail condition))))))
			      (subseq val 0 (position #\) val)))))
    (t condition)))
	    








