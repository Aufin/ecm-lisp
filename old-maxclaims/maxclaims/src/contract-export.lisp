(in-package :maxclaims)

(defvar *export-directory-root* #P"/tmp/contract-export/")

;;; Tables are in a partial order (A is referenced by B). It is manually
;;; done now because the code is needed for one case.

;;; The format is (table id-column (table fkey ...) ...)

;;; todo: add id columns
(defparameter *export-tables*
  `((contract contract-id
	      (risk contract-id))
  
    (risk risk-id
	  (risk-risk-detail risk-id)
	  (claim risk-id))

    (risk-risk-detail risk-risk-detail-id
		      (risk-risk-detail-detail risk-risk-detail-id))
    (risk-risk-detail-detail risk-risk-detail-detail-id)
    (risk-detail risk-detail-id
		 (risk-risk-detail risk-detail-id))
    (risk-detail-type risk-detail-type-id
		      (risk-detail risk-detail-type-id))
    (risk-detail-category category-name
			  (risk-detail-type risk-detail-category-name))

    (claim claim-id
	   (attachment claim-id)
	   (claim-claim-detail claim-id)
	   (claim-transaction claim-id)
	   (timecard claim-id))
    (attachment attachment-id)
    (claim-claim-detail claim-claim-detail-id)
    (claim-transaction transaction-id)
    (timecard timecard-id)
  
   
    (claim-detail claim-detail-id
		  (claim-claim-detail claim-detail-id))
    (claim-detail-type claim-detail-type-id
		       (claim-detail claim-detail-type-id))

    (risk-type type-name
	       (risk risk-type-name)
	       (claim-detail-type risk-type-name)
	       (risk-detail-type risk-type-name))
  
    (claim-transaction-type claim-transaction-type-id
			    (claim-transaction transaction-type-id))
    (claim-transaction-heading claim-transaction-heading-name
			       (claim-transaction transaction-heading))
    (claim-transaction-expense-type claim-transaction-expense-type-name
				    (claim-transaction expense_type))

    (policy policy-id
	    (risk policy-id))

    #+nil(app-user app-user-id
	      (attachment app-user-id)
	      (timecard app-user-id))
  
    (person person-id
	    (claim adjuster-id plaintiff-id)
	    #+nil(app-user person-id)
	    (claim-transaction payee-id)
	    (policy agent-id insured-id company-id
		    underwriter-id branch-id agency-office-id)
	    (contract agency-id insurance-company-id))

    (province-state province-state-id
		    (person province-state-id))))


(defun table-key (table)
  (second (assoc table *export-tables*)))

;;; for each read only contract
;;;  - fetch the contract
;;;  - recursively fetch all related row ids
;;; ((table id ...) ...)

(defun related-data (contract-id)
  (labels ((related-data* (remaining-tables data)
	     #+nil(break "~A ~A" remaining-tables data)
	     (if (null remaining-tables)
		 data
		 (destructuring-bind (table id-column &rest fkeys)
		     (car remaining-tables)
		   (related-data*
		    (cdr remaining-tables)
		    (if-bind ids (cdr (assoc table data))
		      ;; a previous query has fetched applicable
		      ;; rows of the table -> fetch anything
		      ;; referencing it
		      (append (mapcar (lambda (fkey)
					(destructuring-bind (table ref)
					    fkey
					  (cons table
						(query
						 (:select (table-key table)
							  :from table
							  :where (:in ref
								      (:set ids)))
						 :column))))
				      fkeys)
			      data)
		      
		      ;; the table has yet to be fetched, use
		      ;; referencing tables to fetch applicable rows
		      (acons table
			     (let ((all-fkeys (mappend
					       (lambda (fkey)
						 (destructuring-bind (table &rest refs)
						     fkey
						     (mappend (lambda (ref)
								(query (:select ref
										:from table
										:where (:in (table-key table)
											    (:set (cdr (assoc table data)))))
								       :column))
							      refs)))
					       fkeys)))
			       (query (:select id-column
					       :from table
					       :where (:in id-column (:set all-fkeys)))
				      :column))
			     data)))))))
    
    
    (let ((contract (query (:select 'contract-id
				    :from 'contract
				    :where (:= 'contract-id
					       contract-id))
			   :column)))
      (when contract
	(related-data* *export-tables* (acons 'contract contract nil))))))

(defun table->csv (table keys stream)
  (let ((columns (mapcar (compose #'s-sql:to-sql-name #'car)
			 (query (:limit (:select '* :from table) 1)
				:alist))))
    (write-csv-line columns stream)
    (mapc (lambda (row)
	    #+nil(break "~A" row)
	    (write-csv-line (mapcar
			     (lambda (field)
			       (cond ((typep field 'simple-date:timestamp)
				      (local-time:format-timestring
				       nil
				       (local-time:universal-to-timestamp
					(simple-date:timestamp-to-universal-time
					 field))
				       :timezone local-time:+utc-zone+))
				     (t field)))
			     row)
			    stream))
	  (query (:select '* :from table :where (:in (table-key table)
						     (:set keys)))))))



(defun relation-data->csv (data export-directory)
  (dolist (result data)
    (with-open-file (out (merge-pathnames (make-pathname
					   :name (s-sql:to-sql-name (first result))
					   :type "csv")
					  export-directory)
			 :direction :output
			 :if-exists :supersede)
      (table->csv (car result) (cdr result) out))))

(defun copy-claim-attachments (attachment-ids destination-root)
  (when attachment-ids
    (mapc (lambda (attachment)
	    (let ((destination (merge-pathnames (attachment-relative-pathname attachment)
						destination-root)))
	      (ensure-directories-exist destination)
	      (format t "~S ~S~%" (merge-attachment-pathname attachment) destination)
	      (ignore-errors
		#-skip-files(copy-file (merge-attachment-pathname attachment) destination
				       :if-to-exists :error))))
	  (with-dummy-context (:render nil :action nil)
	    (select-objects 'attachment :where `(:in ,(table-key 'attachment)
						     (:set ,@attachment-ids)))))))

(defun export-contract (contract-id export-root)
  (let ((data (related-data contract-id))
	(export-root
	 (merge-pathnames
	  (make-pathname
	   :directory `(:relative ,(query (:select 'contract-number :from 'contract
						   :where (:= 'contract-id contract-id))
					  :single!)))
	  export-root)))
    (ensure-directories-exist export-root)
    (relation-data->csv data export-root)
    (copy-claim-attachments (cdr (assoc 'attachment data))
			    (merge-pathnames (make-pathname
					     :directory `(:relative "attachments"))
					     export-root))))

(defun export-contracts (&optional (contract-ids
				    (query (:select 'contract-id :from 'read-only-contract)
					   :column))
			 (export-root *export-directory-root*))
  (mapc (rcurry #'export-contract export-root)
	contract-ids))