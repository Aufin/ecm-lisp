(in-package :maxclaims)

(defun app-user-log (&key (log-type "VIEW")
		       (log-info "") (row-type "") (row-id 0))
  (with-ldb 
    (select `(:insert-user-log ,row-type, row-id, log-type, log-info))))

