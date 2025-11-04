(cl:in-package #:maxclaims.test)

(defsuite (read-only-contracts :in maxclaims))

(deftest (ro-contract-database-predicate :in read-only-contracts) ()
  (flet ((tuple-read-only-p (tuple key source)
	   ;; prevent unexpected faulures when there are no rows in
	   ;; the set
	   (unless (= (query (sql-compile `(:select (:count ,key)
						  :from ,tuple
						  :where (:in ,key ,source)))
			   :single!)
		      0)
	     (is (eq t (select-one-randomly
			`(:select (:tuple-read-only-p ,(make-symbol
							(format nil "~A.*" tuple)))
				  :from ,tuple
				  :where (:in ,key ,source))))))
	   (unless  (= (query (sql-compile `(:select (:count ,key)
						  :from ,tuple
						  :where (:not (:in ,key ,source))))
			   :single!)
		      0)
	     (is (eq :null (select-one-randomly
			    `(:select (:tuple-read-only-p ,(make-symbol
							    (format nil "~A.*" tuple)))
				      :from ,tuple
				      :where (:not (:in ,key ,source)))))))))
    (with-connection *connect-spec*
      ;; test tuples related to contract
      (tuple-read-only-p 'contract 'contract-id
			 `(:select 'contract_id :from 'read-only-contract))
      (tuple-read-only-p 'risk 'contract_id
			 `(:select 'contract_id :from 'read-only-contract))
      
      ;; test tuples related to risk
      (flet ((risk-tuple-read-only-p (tuple)
	       (tuple-read-only-p tuple 'risk-id
				  `(:select 'risk_id :from 'risk :where (:in 'contract-id (:select 'contract_id :from 'read-only-contract))))))
	(risk-tuple-read-only-p 'claim)
	(risk-tuple-read-only-p 'risk-risk-detail)

	;; note: this fails because there are no
	;; risk-risk-detail-details related to the read only contracts
	(tuple-read-only-p 'risk-risk-detail-detail 'risk-risk-detail-id
			   `(:select 'risk-risk-detail-id :from 'risk-risk-detail :where (:in 'risk-id (:select 'risk_id :from 'risk :where (:in 'contract-id (:select 'contract_id :from 'read-only-contract)))))))
            
      ;; test tuple related to claim
      (flet ((claim-tuple-read-only-p (tuple)
	       (tuple-read-only-p tuple 'claim_id
				  `(:select 'claim_id :from claim :where (:in 'risk-id (:select 'risk_id :from 'risk :where (:in 'contract-id (:select 'contract_id :from 'read-only-contract))))))))
	(claim-tuple-read-only-p 'claim-transaction)
	(claim-tuple-read-only-p 'attachment)
	(claim-tuple-read-only-p 'timecard)
	(claim-tuple-read-only-p 'claim-claim-detail)))))


(deftest (ro-contract-database-trigger :in read-only-contracts) ()
  (let* ((random-ro-contract `(:limit
			       (:order-by (:select 'contract_id
						   :from 'read-only-contract)
					  (:random))
			       1))
	(random-rw-contract `(:limit
			      (:order-by
			       (:select 'contract-id :from 'contract
					:where (:not (:in 'contract_id
							  (:select 'contract_id
								   :from 'read-only-contract))))
			       (:random))
			      1)))
    (with-connection *connect-spec*
      (signals database-error
	(in-transaction
	 (lambda ()
	   (query (sql-compile `(:delete-from 'contract
					      :where (:= 'contract-id
							 ,random-ro-contract)))))))
      (signals database-error
	(in-transaction
	 (lambda ()
	   (query (sql-compile `(:update 'contract
					 :set 'contract_number "foobar"
					 :where (:= 'contract-id
						    ,random-ro-contract)))))))
      (signals database-error
	(in-transaction
	 (lambda ()
	   (query (sql-compile `(:update 'contract
					 :set 'contract_id 555555555
					 :where (:= 'contract-id
						    ,random-ro-contract)))))))
      ;; INSERT contract without ro_contract_id    
      (finishes
	(in-transaction
	 (lambda ()
	   (let ((id (query (:insert-into 'contract
					  :set 'contract-number "foobar"
					  :returning 'contract-id)
			    :single)))
	     (is (string= (query (sql-compile
				  `(:select 'contract-number :from 'contract
					    :where (:= 'contract-id ,id)))
				 :single)
			  "foobar"))))))

      ;; DELETE contract without ro_contract_id
      ;; Checked by hand--this doesn't work most of the time because
      ;; all FKEY references to contract are ON DELETE RESTRICT
      #+nil(finishes
	     (in-transaction
	      (lambda ()
		(let ((id (query (sql-compile
				  `(:delete-from 'contract
						 :where (:= 'contract-id
							    ,random-rw-contract)
						 :returning 'contract-id))
				 :single)))
		  (is (eq (query (sql-compile
				  `(:select 'contract-id :from 'contract
					    :where (:= 'contract-id ,id)))
				 :single)
			  :null))))))

      ;; UPDATE contract without ro_contract_id
      (finishes
	(in-transaction
	 (lambda ()
	   (let ((id (query (sql-compile
			     `(:update 'contract
				       :set 'contract-number "foobar"
				       :where (:= 'contract-id
						  ,random-rw-contract)
				       :returning 'contract-id))
			    :single)))
	     (is (string= (query (sql-compile
				  `(:select 'contract-number :from 'contract
					    :where (:= 'contract-id ,id)))
				 :single)
			  "foobar"))))))

      ;; INSERT risk with ro-contract_id
      (signals database-error
	(in-transaction
	 (lambda ()
	   (query (sql-compile
		   `(:insert-into 'risk
				  :set 'contract-id ,random-ro-contract))))))
	
      ;; The following cases shouldn't need to be tested (the
      ;; predicates were already checked, and these cases are
      ;; checked by attempting them on `contract')
	
      ;; INSERT risk without ro-contract_id
      ;; UPDATE risk with ro-contract_id
      ;;        -> SET policy_id = random policy
      ;;        -> SET contract_id = 5555555
      ;; UPDATE risk without ro-contract_id
      ;;        -> SET policy_id = random policy
      ;;        -> SET contract_id = random ro-contract_id
      ;; DELETE risk with ro-contract_id
      ;; DELETE risk without ro-contract_id
      )))

(deftest (ro-contract-lisp-predicate :in read-only-contracts) ()
  (flet ((tuple-read-only-p (type key source)
	   ;; skip test when there are no records to test
	   (arnesi:when-bind o
	       (car (rofl::query-objects
			      type
			      (lambda (table fields)
				`(:limit (:order-by
					  (:select ,@fields
						   :from ,table
						   :where (:in ,key ,source))
					  (:random))
					 1))))
	     (is (eq nil (maxclaims::contract-writeable-p o))))
	   (arnesi:when-bind o
	       (car (rofl::query-objects
			    type
			    (lambda (table fields)
			      `(:limit (:order-by
					(:select ,@fields
						 :from ,table
						 :where (:not (:in ,key ,source)))
					(:random))
				       1))))
	     (is (eq t (maxclaims::contract-writeable-p o))))))
    (with-connection *connect-spec*
      ;; test tuples related to contract
      (tuple-read-only-p 'maxclaims::contract 'contract-id
			 `(:select 'contract_id :from 'read-only-contract))
      (tuple-read-only-p 'maxclaims::risk 'contract_id
			 `(:select 'contract_id :from 'read-only-contract))
      
      ;; test tuples related to risk
      (flet ((risk-tuple-read-only-p (tuple)
	       (tuple-read-only-p tuple 'risk-id
				  `(:select 'risk_id :from 'risk :where (:in 'contract-id (:select 'contract_id :from 'read-only-contract))))))
	(risk-tuple-read-only-p 'maxclaims::claim)
	(risk-tuple-read-only-p 'maxclaims::risk-risk-detail)

	;; note: this fails because there are no
	;; risk-risk-detail-details related to the read only contracts
	(tuple-read-only-p 'maxclaims::risk-risk-detail-detail 'risk-risk-detail-id
			   `(:select 'risk-risk-detail-id :from 'risk-risk-detail :where (:in 'risk-id (:select 'risk_id :from 'risk :where (:in 'contract-id (:select 'contract_id :from 'read-only-contract)))))))
            
      ;; test tuple related to claim
      (flet ((claim-tuple-read-only-p (tuple)
	       (tuple-read-only-p tuple 'claim_id
				  `(:select 'claim_id :from claim :where (:in 'risk-id (:select 'risk_id :from 'risk :where (:in 'contract-id (:select 'contract_id :from 'read-only-contract))))))))
	(claim-tuple-read-only-p 'maxclaims::claim-transaction)
	;; fixme: not testing attachment as an :initform depends on
	;; *context*, and the dummy context is not setup properly and
	;; fails
	#+nil(ucw-core:with-dummy-context ()
	  (claim-tuple-read-only-p 'maxclaims::attachment))
	(claim-tuple-read-only-p 'maxclaims::timecard)
	(claim-tuple-read-only-p 'maxclaims::claim-claim-detail))

      (dolist (type '(maxclaims::contract 
		      maxclaims::risk 
		      maxclaims::risk-risk-detail 
		      maxclaims::risk-risk-detail-detail 
		      maxclaims::claim 
		      maxclaims::claim-transaction 
		      #+nil maxclaims::attachment 
		      maxclaims::timecard 
		      maxclaims::claim-claim-detail))
	(is (eq t (maxclaims::contract-writeable-p (make-instance type))))
	
	#+nil(finishes
	  (let ((id-column-name (rofl::class-id-column-name (find-class type))))
	    (dolist (id (query (sql-compile `(:select ,id-column-name
					      :from ,type))
			       :column))
	      
	      (maxclaims::contract-writeable-p
	       (car (select-objects type :where `(:= ,id-column-name ,id)))))))))))
