(in-package :rofl)

;;;; NB: These could really be in upstream


(defun plist-row-reader (stream fields)
  (postmodern::symbol-plist-row-reader stream fields))

(defclass bytea-stream (trivial-gray-streams:trivial-gray-stream-mixin 
			trivial-gray-streams:FUNDAMENTAL-BINARY-INPUT-STREAM
			trivial-gray-streams:FUNDAMENTAL-BINARY-OUTPUT-STREAM)

	((table-name 
	  :accessor bytea-stream-table-name 
	  :initarg :table)
	 (column-name
	  :accessor bytea-stream-column-name
	  :initarg :column)
	 (primary-key-name
	  :accessor bytea-stream-primary-key-name
	  :initarg :primary-key-name)
	 (primary-key-value
	  :accessor bytea-stream-primary-key-value
	  :initarg :primary-key-value)
	 (stream-position 
	  :accessor trivial-gray-streams:stream-file-position
	  :initform 0)
	 (stream-length 
	  :accessor bytea-stream-length)))

(defun set-bytea-stream-length-from-database (stream)
  (setf (bytea-stream-length stream)
	(second (select-only 1 `(:length ,(bytea-stream-column-name stream)) 
			     :from (bytea-stream-table-name stream)
			     :where `(:= ,(bytea-stream-primary-key-name stream)
					 ,(bytea-stream-primary-key-value stream))))))

(defmethod shared-initialize :after ((stream bytea-stream) slots &rest args)
  (declare (ignore slots args))
  (set-bytea-stream-length-from-database stream))

(defmethod trivial-gray-streams:stream-write-sequence ((stream bytea-stream) sequence start end &key &allow-other-keys)
  (let ((seq (make-array (- end start) 
			 :displaced-to (if (listp sequence)

					   (coerce sequence 'vector)
					   sequence)
			 :displaced-index-offset start
			 :element-type '(unsigned-byte 8))))
         
    (ensure-transaction ()      

      (update (bytea-stream-table-name stream) 
	      :set  (bytea-stream-column-name stream) `(:\|\| (:coalesce ,(bytea-stream-column-name stream) "")  ,seq)
	      :where `(:= ,(bytea-stream-primary-key-name stream)
			  ,(bytea-stream-primary-key-value stream)))
      (set-bytea-stream-length-from-database stream))
    sequence))

(defmethod trivial-gray-streams:stream-read-sequence ((stream bytea-stream) sequence start end &key &allow-other-keys)

  (let* ((length (- end start))
	 (data (second (select-only 1 (list :substring (bytea-stream-column-name stream)
					    :from 
					    (1+ (trivial-gray-streams:stream-file-position stream))
					    :for length)
				    :from (bytea-stream-table-name stream)
				    :where (list := (bytea-stream-primary-key-name stream)
						 (bytea-stream-primary-key-value stream))))))

    (if (not  (eq :null data)) 
      (loop 
	 :for i :from 0 :to (1- length)
	 :for byte :across data
	 :do (setf (elt sequence i) byte)
	 :finally     (incf (trivial-gray-streams:stream-file-position stream) length)

	 (return i)
	 )
      0)))

(defmethod sb-gray::stream-element-type ((stream bytea-stream))
  '(unsigned-byte 8))
					 

(s-sql::def-sql-op :substring (string &key (from 0) for)
  `("SUBSTRING(" ,@(s-sql::sql-expand string) " FROM " ,@(s-sql::sql-expand from) ,@(when for `(" FOR " ,@(s-sql::sql-expand for) ")"))))

;;;; now the rofl code itself

(defparameter *row-reader* 'plist-row-reader)
(defparameter *trace-query* nil)

(defun %query (query &optional parameters)
					;  (break "~A" query)
  #+ (or) (eval `(postmodern:query 
		  ,(sql-compile query) 
		  ,@parameters
		  :plist
		  ))
  #+ (or )  (cl-postgres:exec-query *database* (funcall 
						(if *trace-query* 
						    (lambda (thing)
						      (print thing *trace-output*))
						    #'identity)
						(sql-compile query)
						) *row-reader*)
  (if parameters 
      (progn 
	(cl-postgres:prepare-query 
	 *database* "" (sql-compile query))
	(cl-postgres:exec-prepared 
	 *database* "" parameters *row-reader*))
      (cl-postgres:exec-query 
       *database* 
       (sql-compile query) *row-reader*))
  )


(defun select (&rest query)
  (%query (cons :select query)))

#+(or)(defun prepare (&rest query)
  (cl-postgres:prepare-query *database* "test2" (sql-compile (cons :select query))))


(defun select-only* (limit offset &rest query)
  (let ((results (%query `(:limit ,(cons :select query) ,limit ,offset))))
    (if (eql 1 limit)
	(first results)
	results)))

(defun select-only (num &rest query)
  (apply #'select-only* num nil query))

(defun insert-into (table &rest values-plist)
  (postmodern:execute 
   (postmodern:sql-compile `(:insert-into ,table :set ,@values-plist))))

(defun update (table &rest query)
  (postmodern:execute 
   (postmodern:sql-compile `(:update ,table ,@query))))

(defun delete-from (table &rest query)
  (postmodern:execute 
   (postmodern:sql-compile `(:delete-from ,table ,@query))))
    






