(in-package #:maxclaims)

(defparameter *slow-query-cache* (make-hash-table :synchronized t))

(defstruct slow-query-cache-entry
  (lock (bt:make-lock))
  (cache))

(defun slow-query-cache-ref (key &key populate (wait t))
  (let ((entry
	 (sb-ext:with-locked-hash-table (*slow-query-cache*)
	   (if-bind entry (gethash key *slow-query-cache*)
	     entry
	     (when (functionp populate)
	       (let* ((e (make-slow-query-cache-entry))
		      (entry-lock (slow-query-cache-entry-lock e))
		      ;; a semaphore is used because condvars require
		      ;; the lock to be held for wait/notify, but notify
		      ;; neither blocks when the waitqueue is empty nor
		      ;; releases the lock making them insufficient to
		      ;; prevent a race for the cache entry lock.
		      (started (sb-thread:make-semaphore)))
		 (bt:make-thread (lambda ()
				   (with-db
				     (bt:with-lock-held (entry-lock)
				       ;; races are not permitted in this county
				       #+nil(Break "before sig")
				       (sb-thread:signal-semaphore started)
				       #+nil(break "after sig")
				       (setf (slow-query-cache-entry-cache e)
					     (funcall populate)))))
				 :name (format nil "~A-cache-populator" key))
		 (sb-thread:wait-on-semaphore started)
		 #+nil(break "after wait release")
		 (setf (gethash key *slow-query-cache*) e)))))))
    (if entry
	(let ((locked))
	  (unwind-protect (progn (setf locked
				       (bt:acquire-lock (slow-query-cache-entry-lock
							 entry)
							wait))
				 (if locked
				     (values (slow-query-cache-entry-cache entry)
					     t)
				     (values nil nil)))
	    (when locked (bt:release-lock (slow-query-cache-entry-lock entry)))))
	(values nil nil))))

(defun delete-records-from-cache (key records)
  (when-bind entry (gethash key *slow-query-cache*)
    (bt:with-lock-held ((slow-query-cache-entry-lock entry))
      (setf (slow-query-cache-entry-cache entry)
	    (remove-if (lambda (r) (member r records :test #'db=))
		       (slow-query-cache-entry-cache entry))))))