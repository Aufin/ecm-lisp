(in-package #:maxclaims)

;;; Search Engine Functions
(defun %contract-id (contract)
  (and contract (contract.contract-id contract)))
(defun existing (in-table column-name column-table)
  (let* ((existing (postmodern:query 
		    (:select (:distinct column-name) 
			     :from in-table) :column)))
    (query-objects 
     column-table  
     (lambda (name cols) 
       `(:order-by 
	(:select 
	 ,@cols :from ,name
	 :where (:in  person-id (:set  ,@(remove :null existing))))
        (person_name person-id))))))

(defgeneric remove-duplicates* (list item))

(defun remove-duplicate-records (list)
  (remove-duplicates* list (first list)))
  
(defun claim-access-p (claim)
  (let* ((claims-access
	         (with-adb
	           (select 'access
		                 'claim-id
		                 :from 'app-user-claim 
		                 :where `(:= app-user-id 
			                           ,(app-user.app-user-id $app-user)))))
	       (access 
	         (find (claim.claim-id claim)  
		             claims-access
		             :key (lambda (c)
		                    (getf c :claim-id)))))
    (unless (and access 
		             (not (getf access :access)))
      T))(defmethod remove-duplicates* (list record)
    (remove-duplicates
     list :key #'object-id :from-end t))
 

  )


(defun filter-claims (claims)
  (let* ((contract-ids (mapcar #'%contract-id
			       (user-contracts $app-user)))
	 )

    (if (null contract-ids)
	claims
	(remove-if-not (lambda (claim)
			             (or (member (ignore-errors (%contract-id (risk.contract (claim.risk claim))))
				                       contract-ids)
			                 (claim-access-p claim)))
		       claims))))


(defun string-search-claim (search-string &key (exact nil) 
					                                  (limit '(:raw "ALL")))
  (filter-claims
   (append
    (let ((res (ignore-errors (parse-integer search-string :junk-allowed nil))))
      (when res
        (append 
	       (select-only-n-objects 
	        limit 
	        'claim
	        :where `(:= claim-id ,res))
	       (unless  exact
	         (ignore-errors
                                        ;might be out-of-range
                                        ;todo: catch the actual condition
	          (select-objects 
	           'claim 
	           :where  `(:or 
		                   (:= claim-id ,res)
		                   (:= claim-id (:* ,res 10))
		                   (:= claim-id (:* ,res 10))
		                   (:= claim-id (:* ,res 100))
		                   (:= claim-id (:* ,res 1000))
		                   (:= claim-id (:* ,res 10000))))))))))))

(defun filter-policies (policies)
  (let ((contract-ids (mapcar #'%contract-id
			      (user-contracts $app-user))))
    (if (null contract-ids)
	policies
	(remove-if-not (lambda (policy)
			 (intersection 
			  (mapcar (lambda (r)
				    (%contract-id 
				     (risk.contract r)))
				  (find-risks policy))
			  contract-ids)) 
		       policies))))

(defun string-search-policy (search-string  &key (exact nil))
 ;; (break "~A" search-string)
  (let* ((policies (select-objects 
		   'policy 
		   :where `(:ilike  policy-number ,search-string))))
    (filter-policies 
     (append 
      policies
      (unless (or policies exact)
	(select-only-n-objects 
	 25  
	 'policy 
	 :where `(:or (:ilike 
		       policy-number 
		       ,(format nil "%~A%" search-string))
		      (:% policy-number ,search-string))))))))

(defun filter-contracts (contracts)
  (let ((contract-ids (mapcar #'%contract-id
			      (user-contracts $app-user))))
    (if (null contract-ids)
	contracts
	(remove-if-not (lambda (contract)
			 (member (%contract-id contract) contract-ids))
		       contracts))))

(defun string-search-contract (search-string  &key (exact nil))
  (when (not (equalp "" search-string)) 
    (let ((contracts (select-objects 'contract 
				     :where `(:ilike contract_number
						 ,search-string))))
      
      (filter-contracts  
       (append 
	contracts
	(unless (and exact contracts) 
	  (select-only-n-objects 25 'contract 
				 :where `(:and 
					  (:not (:= contract_number
						    ,search-string))
					  (:or
					   (:ilike 
					    contract-number 
					    ,(format nil "%~A%" search-string)))))))))))


(defun filter-people (people)
  (let ((contract-ids (mapcar #'%contract-id
			      (user-contracts $app-user))))
    (if (null contract-ids)
	people
	(nconc  (remove-if-not (lambda (person)
				 (intersection 
				  (mapcar (lambda (r)
					    (when (slot-boundp r 'contract-id)
					      (slot-value r 'contract-id)))
					  (agency-contracts person))
				  contract-ids))
			       people)
		(remove-if-not (lambda (person)
				 (intersection 
				  (mapcar (lambda (risk)
					    (%contract-id (risk.contract risk)))
					  (mapcan #'find-risks (person.policies-as-insured person)))
				  contract-ids))
			       people)))))

(defun string-search-person (search-string &key (exact nil))
;; (break)
  (when (not (equalp "" search-string))
    (let ((names (split-sequence #\Space search-string))) 
      (filter-people  
       (remove-duplicates 
	(append 
	 (let* ((ex (select-objects 
		     'person 
		     :where `(:or
			  ,@ (when (second names)
			       (list
				`(:and
				  (:ilike first-name ,(first names))
				  (:ilike last-name ,(second names)))))
			     
			  (:ilike first-name ,search-string)
			  (:ilike last-name ,search-string)
			  (:ilike company-name ,search-string)
			  (:ilike (person-name (:dot person *)) ,search-string))))
	      (ex1  (select-objects 
		      'person 
		      :where 
		      `(:or
			,@(when (second names)
			     (list
			      `(:and
				(:ilike 
				 first-name 
				 ,(first names))
				(:ilike 
				 last-name 
				 ,(second names)))))		       
			(:ilike 
			 first-name 
			 ,(format nil "~A%" search-string))
			(:ilike 
			 last-name 
			 ,(format nil "~A%" search-string))
			(:ilike 
			 company-name 
			 ,(format nil "~A%" search-string))))))
	  
	  (let ((d (remove-duplicates (append ex ex1)
			     :key #'person.person-id
			     :from-end t)))
	    (or d
		(select-objects 
		 'person 
		 :where `(:ilike 
			 company-name 
			 ,(format nil "%~A%" search-string))))))
		 
	(unless exact 	
	  (select-objects 
	   'person 
	   :where `(:or
		    ,@ (when (second names)
			 (list
		       `(:and
			 (:or
			  (:ilike 
			   first-name 
			   ,(format nil "%~A%" (first names)))
			  (:% first-name ,(first names)))
			 (:or
			  (:ilike 
			   last-name 
			   ,(format nil "%~A%" (second names)))
			  (:% last-name ,(second names))))))
		 (:ilike 
		  first-name 
		  ,(format nil "%~A%" search-string))
		 (:ilike 
		  last-name 
		  ,(format nil "%~A%" search-string))
		 (:ilike 
		  company-name 
		  ,(format nil "%~A%" search-string))
		 (:% first-name ,search-string)
		 (:% last-name ,search-string)
		 (:% company-name ,search-string)
		 (:% (person-name (:dot person *)) ,search-string)))))
	:from-end t
	:key #'person.person-id)))))

(defun string-search-agency (string  &key &allow-other-keys)
  (remove-if-not #'agency-contracts (string-search-person string)))

(defun filter-claim-transactions (claim-transactions)
  (let ((contract-ids (mapcar #'%contract-id
			      (user-contracts $app-user))))
    (if (null contract-ids)
	claim-transactions
	(remove-if-not (lambda (claimt)
			 (or (claim-access-p (claim-transaction.claim claimt))
			     (member (%contract-id
				  (risk.contract 
				   (claim.risk 
				    (claim-transaction.claim claimt))))
				 contract-ids)))
		       	claim-transactions))))

(defun filter-claim-timecards (claim-tcs)
  (let ((contract-ids (mapcar #'%contract-id
			      (user-contracts $app-user))))
    (if (null contract-ids)
	claim-tcs
	(remove-if-not (lambda (claimt)
			 (or (claim-access-p (timecard.claim claimt))
			 (member (%contract-id
				  (risk.contract 
				   (claim.risk 
				    (timecard.claim claimt))))
				 contract-ids)))
		       	claim-tcs))))

(defun filter-claim-attachments (claim-scs)
  (let ((contract-ids (mapcar #'%contract-id
			      (user-contracts $app-user))))
    (if (null contract-ids)
	claim-scs
	(remove-if-not (lambda (claima)
			 (or (claim-access-p (attachment.claim claima))
			     (member (%contract-id
				      (risk.contract 
				       (claim.risk 
					(attachment.claim claima))))
				     contract-ids)))
			 claim-scs))))

(defun filter-claim-diaries (claim-ds)
  (let ((contract-ids (mapcar #'%contract-id
			      (user-contracts $app-user))))
    (if (null contract-ids)
	claim-ds
	(remove-if-not (lambda (claimd)
			 (member (%contract-id
				  (risk.contract 
				   (claim.risk 
				    (diary-entry.claim claimd))))
				 contract-ids))
		       	claim-ds))))

(defun filter-risks (risks)
  (let ((contract-ids (mapcar #'%contract-id
			      (user-contracts $app-user))))
    (if (null contract-ids)
	risks 
	(remove-if-not (lambda (risk)
			 (member (%contract-id (risk.contract risk))
				 contract-ids))
		       risks))))

(defun filter-objects (objects)
  (funcall 
   (typecase (first objects)
     (claim #'filter-claims)
     (loss-detail #'identity)
     (driver-detail #'identity)
     (vehicle-detail #'identity)
     (claim-status-detail #'identity)
     (policy #'filter-policies)
     (policy-detail #'identity)
     (person #'filter-people)
     (contract #'filter-contracts)
     (contract-authority #'identity)
     (claim-transaction #'filter-claim-transactions)
     (timecard #'filter-claim-timecards)
     (timecard-interim #'identity)
     (attachment #'filter-claim-attachments)
     (diary-entry #'filter-claim-diaries)
     (app-user #'identity)
     (app-adjuster #'identity)
     (app-user-contract #'identity)
     (app-user-agency #'identity)
     (app-user-syndicate #'identity)
     (app-user-claim #'identity)
     (risk #'filter-risks)
     (risk-risk-detail #'identity)
     (claim-claim-detail #'identity)
     (claim-update #'identity)
     (claim-authority #'identity)
     (null #'identity)     
     (t (error (format NIL "No Access to ~A, has been declined. Please contact the system administrator for further details" (type-of (first objects))))))
   objects))
  
(defun search-records (search-string &key exact
				       ((:using search-functions) 
					'(string-search-claim
					  string-search-policy
					  string-search-person
					  string-search-contract)))
  (mapcan (rcurry #'funcall search-string :exact exact) search-functions))
