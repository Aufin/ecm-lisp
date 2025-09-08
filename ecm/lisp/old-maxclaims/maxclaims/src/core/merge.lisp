(in-package :maxclaims)

;;; External Protocol
(defgeneric merge.master-table (merge))
(defgeneric merge.deleted-record (merge))
(defgeneric merge.constraints (merge))

(defun effective-merged-id (merge)
  (loop
     :with current-id = (merge-history.into-id merge)
     :for next-object = (select-only-n-objects 1 (class-name (class-of merge))
		   :where `(:= from-id ,current-id))
     :while next-object
     :do (progn ;(break "c = ~A, n = ~A" current-id next-object)
		(setf current-id (merge-history.into-id next-object)))
     :finally (return current-id)))

(defun merge-table-name (merge foreign-table foreign-key)
  (make-symbol (string-upcase (format nil "~A-merge-~A/~A"
				      (merge.master-table merge)
				      foreign-table foreign-key))))