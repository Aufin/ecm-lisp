;;; Searching for likely duplicates

(in-package :maxclaims)

;;; Potential Duplicates of a Record
(defgeneric find-likely-duplicates (master-record
				    &optional selected-duplicates)
  (:documentation "Return likely duplicates of `master-record'
  filtering out members of `selected-duplicates' if provided."))

;;; todo: add person_name % person_name to use index
(defmethod find-likely-duplicates ((contract contract)
				   &optional selected-duplicates)
  (remove-if (rcurry #'member selected-duplicates :test #'db=)
	     (select-objects 'contract
			     :where `(:and (:not (:= 'contract-id
						     ,(contract.contract-id contract)))

					   (:= (:extract :year 'effective-date)
					       (:extract :year ,(contract.effective-date contract)))
					   (> (similarity 'contract-number
							  ,(contract.contract-number contract))
					      0.74)))))

(defmethod find-likely-duplicates ((policy policy)
				   &optional selected-duplicates)
  (remove-if (rcurry #'member selected-duplicates :test #'db=)
	     (select-objects 'policy
			     :where `(:and (:not (:= 'policy-id
						     ,(policy.policy-id policy)))

					   (:= (:extract :year 'effective-date)
					       (:extract :year ,(policy.effective-date policy)))
					   (> (similarity 'policy-number
							  ,(policy.policy-number policy))
					      0.74)))))

(defun %person-name-sql (person)
  `(:or (:and  (> (:char-length 
		   (person-name 
		    (find-person ,(person.person-id person))))
		  1)
	       (> (:char-length 
		   (person-name 'person.*))
		  1)
	       (> (similarity 
		   (person-name (find-person ,(person.person-id person)))
		   (person-name 'person.*))
		  0.74))

	(:and 
	 (> (:char-length (person-short-name 
			   (find-person ,(person.person-id person)))) 1)
	 (> (similarity (person-short-name 'person.*)
		       (person-short-name (find-person ,(person.person-id person))))
	   0.90))))

(defmethod find-likely-duplicates ((person person)
				   &optional selected-duplicates)
  ;;(break "~A" 'deplicat)
  (remove-if (rcurry #'member selected-duplicates :test #'db=)
	     (select-objects 'person
			     :where `(:and (:not (:= 'person-id
						     ,(if (slot-boundp person 'person-id)
							  (person.person-id person)
							  0)))
					   ,(%person-name-sql person)))))


;;; All records with likely duplicates

(defgeneric find-all-duplicated-records (table-name &key wait))

(defun duplicated-cache-entry-key (table-name)
  (intern (symbol-name (symbolicate "all-duplicated-" table-name))))

(defmethod find-all-duplicated-records :around (table-name &key wait)
  (slow-query-cache-ref (duplicated-cache-entry-key table-name)
			:populate #'call-next-method
			:wait wait))

(defmethod find-all-duplicated-records ((table-name (eql 'policy)) &key wait)
  (declare (ignorable wait))
  (rofl::query-objects
   'policy
   (lambda (table fields)
     `(:raw
       ,(concatenate 'string
		     "WITH p AS 
                  (SELECT policy_id AS pid,
                          policy_number AS pnum,
                          effective_date AS pyear
                   FROM policy)
"
		     (sql-compile
		      `(:order-by
			(:select ,@fields
				 :from ,table 'p
				 :where (:and (:!= 'policy-id 'p.pid)
					      (:= (:extract :year 'effective-date)
						  (:extract :year 'p.pyear))
					      (:% 'policy_number 'p.pnum)
					      (:> (similarity 'policy-number
							      'p.pnum)
						  0.74)))
			'policy-number)))))))

(defmethod find-all-duplicated-records ((table-name (eql 'contract)) &key wait)
  (declare (ignorable wait))
  (rofl::query-objects
   'contract
   (lambda (table fields)
     `(:raw
       ,(concatenate 'string
		     "WITH c AS 
                  (SELECT contract_id AS cid,
                          contract_number AS cnum,
                          effective_date AS cyear
                   FROM contract)
"
		     (sql-compile
		      `(:order-by
			(:select ,@fields
				 :from ,table 'c
				 :where (:and (:!= 'contract-id 'c.cid)
					      (:= (:extract :year 'effective-date)
						  (:extract :year 'c.cyear))
					      (:% 'contract-number 'c.cnum)
					      (:> (similarity 'contract-number
							      'c.cnum)
						  0.74)))
			'contract-number)))))))

(defmethod find-all-duplicated-records ((table-name (eql 'person)) &key wait)
  (declare (ignorable wait))
  (rofl::query-objects
   'person
   (lambda (table fields)
     `(:raw
       ,(concatenate 'string
		     "WITH p AS 
                  (SELECT person_id AS pid,
                          person_name(person.*) AS pname
                   FROM person)
"		     (sql-compile
		      `(:order-by
			(:select ,@fields
				 :from ,table 'p
				 :where (:and (:!= 'person-id 'p.pid)
					      (:% (person_name ,(symbolicate 'person ".*")) 'p.pname)
					      (:> (similarity (person_name ,(symbolicate 'person ".*")) 'p.pname)
						  0.90)))
			(person_name ,(symbolicate 'person ".*")) 'p.pname)))))))

