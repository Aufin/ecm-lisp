(in-package :maxclaims)

;; (merge-duplicate-records :from DUP :into ORIGINAL) seems like it
;; would be easier to use than remembering whether the first or second
;; argument is the duplicate
(defun merge-duplicate-records (&key from into user dry-run)
  (merge-duplicate-records* into from :dry-run dry-run :user user))

(defgeneric merge-duplicate-records* (into duplicate &key user dry-run))

(defun record-merge-item (merge table fkey)
  (let* ((merge-table (merge-table-name merge table fkey))
	 (merge-key (merge.table.key merge table))
	 (records (query (:select merge-key :from table
				  :where (:= fkey (merge-history.from-id merge)))
			 :column)))
    (maxclaims.merge.debug
     "[~A] Merging Table ~A (PKey ~A, FKey ~A); from ~A into ~A. records = ~A"
     (merge.master-table merge)
     table
     (merge.table.key merge table)
     fkey
     (merge-history.from-id merge)
     (merge-history.into-id merge)
     records)
    (mapc (lambda (id)
	    (maxclaims.merge.dribble "Recording change ~A (~A)"
				     merge-table
				     id)
	    (execute (:insert-into merge-table
				   :set 'merge-id (merge-history.merge-id merge)
				   merge-key id)))
	  records)))

(defun merge-table (merge table)
  (flet ((merge-table/key (fkey)
	   (record-merge-item merge table fkey)
	   (query (:update table :set fkey (merge-history.into-id merge)
			   :where (:= fkey (merge-history.from-id merge))
			   :returning (merge.table.key merge table))
		  :column)))
    (mappend #'merge-table/key
	     (merge.table.foreign-keys merge table))))

(defmethod merge-duplicate-records* (into duplicate &key user dry-run)
  (assert (eq (class-of into) (class-of duplicate)))
  (ensure-transaction (merge-transaction)
    (let ((merge (insert-object
		  (make-object (merge-class into)
			       :from-id (object-id duplicate)
			       :into-id (object-id into)
			       :app-user-id (if (typep user 'app-user)
						(app-user.app-user-id user)
						:null)))))
      (insert-object (make-deleted-record duplicate))
      (multiple-value-prog1
	  (values merge (mapcar (lambda (table)
				  (list* (table.name table)
					 (table.key table)
					 (merge-table merge (table.name table))))
				(merge.constraints merge)))
	(delete-object duplicate)
	(when dry-run (abort-transaction merge-transaction))))))

(defun find-records-to-merge (duplicate)
  (ensure-transaction ()
    (let ((dummy-merge (make-object (merge-class duplicate)
				    :from-id (object-id duplicate))))
      
      (mapcar (lambda (table)
		(list*
		 (table.name table)
		 (table.key table)
		 (mappend (lambda (fkey)
			    (query
			     (:select (merge.table.key dummy-merge (table.name table))
				      :from (table.name table) 
				      :where (:= fkey (merge-history.from-id dummy-merge)))
			     :column))
			  (merge.table.foreign-keys dummy-merge (table.name table)))))
	      (merge.constraints dummy-merge)))))