(in-package :maxclaims)

;;; todo: sane return values
(defgeneric unmerge-duplicate-record (merge))

(defun find-records-to-unmerge (merge table table-key fkey)
  (let ((table-ids (query (:select table-key
				  :from (merge-table-name merge table fkey)
				  :where (:= 'merge-id (merge-history.merge-id merge)))
			  :column)))
    (values
     (query (sql-compile `(:select ,table-key
				   :from ,table
				   :where (:and (:= ,fkey ,(effective-merged-id merge))
						(:in ,table-key
						     (:set ,@table-ids)))))
	    :column)
     table-ids)))


(defun unmerge-table (merge table table-key fkeys)
  (mapcar
   (lambda (fkey)
     (let ((records (find-records-to-unmerge merge table table-key fkey))
	   (into-id (effective-merged-id merge)))
       (maxclaims.merge.debug
	"[~A] Unmerging Table ~A (PKey ~A, FKey ~A); into ~A from ~A. records = ~A"
	(merge.master-table merge)
	table
	table-key
	fkey
	(merge-history.from-id merge)
	into-id
	records)
       (mapc (lambda (id)
	       (maxclaims.merge.dribble "Undoing merge ~A <- ~A"
					(merge-history.from-id merge)
					into-id)
	       (execute (:update table :set fkey (merge-history.from-id merge)
				 :where (:and (:= fkey into-id)
					      (:= table-key id)))))
	     records)))
   fkeys))

(defmethod unmerge-duplicate-record ((merge merge-history))
  (ensure-transaction ()
    (undelete-record (merge.deleted-record merge))
    (multiple-value-prog1
	(mapc (lambda (table-and-keys)
		(unmerge-table merge
			       (table.name table-and-keys)
			       (table.key table-and-keys)
			       (table.foreign-keys table-and-keys)))
	      (merge.constraints merge))
      (delete-object merge))))