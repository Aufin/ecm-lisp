(in-package :maxclaims)

;;; When adding a new mergaeable table the database must be manually
;;; updated

(defun construct-merge-table-name (master-table foreign-table foreign-key)
  (make-symbol (string-upcase (format nil "~A-merge-~A/~A"
				      master-table foreign-table foreign-key))))

(defun master-merge-table-name (master-table)
  (make-symbol (string-upcase (format nil "~A-merge" master-table))))

(defun generate-master-merge-table (master-table)
  (sql-compile `(:create-table ,(master-merge-table-name master-table)
			       ((merge-id :type serial :primary-key t)
				(from-id :type integer)
				(into-id :type integer)
				(modification-time :type timestamp :default (now))
				(app-user-id :type (or db-null integer)
					     :default :null
					     :references (app-user)))
			       (:unique 'from-id 'into-id))))

(defun generate-history-tables (master-table)
  (let ((master-merge-table (master-merge-table-name master-table)))
    (cons
     (generate-master-merge-table master-table)
     (mapcan
      (lambda (table)
	(mapcar (lambda (fkey)
		  (sql-compile `(:create-table
				 ,(construct-merge-table-name master-table
							      (table.name table)
							      fkey)
				 ((merge-id :type integer
					    :references (,master-merge-table
							 :cascade
							 :restrict))
				  (,(table.key table)
				    :type integer))
				 (:unique 'merge-id ,(table.key table)))))
		(table.foreign-keys table)))
      (table-constraints master-table)))))

(defun drop-merge-history-tables (master-table)
  (cons (sql-compile `(:drop-table ,(master-merge-table-name master-table)))
   (mappend (lambda (table)
	      (mapcar (lambda (fkey)
			(sql-compile
			 `(:drop-table
			   ,(construct-merge-table-name master-table
							(table.name table)
							fkey))))
		(table.foreign-keys table)))
	   (table-constraints master-table))))

(defun dump-history-tables (tables output-path)
  (with-open-file (out output-path
		       :direction :output
		       :if-exists :supersede)
    (mapc (lambda (table)
	    (format out "~A;~%" table))
	  tables)))
