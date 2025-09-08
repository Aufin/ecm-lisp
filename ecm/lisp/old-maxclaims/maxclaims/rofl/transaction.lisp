(in-package #:rofl)

;;; Extensions of postmodern transaction macros to permit saner
;;; nesting of transactions

(defvar *current-transaction*)

(defmacro with-transaction* ((&optional name) &body body)
  "Equivalent to `postmodern:with-transaction' except that it also
binds `*current-transaction*'."
  (let ((name (or name (gensym))))
    `(with-transaction (,name)
       (let ((*current-transaction* ,name))
	 ,@body))))

(defmacro ensure-transaction ((&optional name) &body body)
  "Either begins a new transaction, or joins an existing transaction
started by `with-transaction*'. When provided, `name' is bound to the
transaction."
  (let ((name (or name (gensym)))
	(fun (gensym)))
    `(let ((,fun (lambda (,name)
		   (declare (ignorable ,name))
		   ,@body)))
       (if (boundp '*current-transaction*)
	   (funcall ,fun *current-transaction*)
	   (with-transaction* (,name)
	     (funcall ,fun ,name))))))