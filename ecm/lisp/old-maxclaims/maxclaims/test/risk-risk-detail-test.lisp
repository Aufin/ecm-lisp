(cl:in-package #:maxclaims.test)

(defsuite (risk-risk-details :in maxclaims))

(deftest (risk-risk-detail-delete-cascade :in risk-risk-details) ()
  (with-connection *connect-spec*
    (let* ((risk-detail-id (select-one-randomly
			    `(:select 'risk-detail-id :from 'risk-detail
				      :where (:= 'detail-detail-type "text"))))
	   (risk-type-name (query
			    (:select 'risk-type-name
				     :from 'risk-detail-type
				     :where (:= 'risk-detail-type-id
						(:select 'risk-detail-type-id
							 :from 'risk-detail
							 :where (:= 'risk-detail-id risk-detail-id))))
			    :single!)))
      (finishes
	(in-transaction
	 (lambda ()
	   ;; create a new risk where risk-risk-detail-details of type TEXT
	   ;; are permitted
	   ;; - random contract+policy
	   (let* ((risk-id (query
			    (:insert-into 'risk
					  :set 'risk-type-name risk-type-name
					  'policy-id (select-one-randomly `(:select 'policy-id :from policy))
					  'contract-id (select-one-randomly `(:select 'contract-id :from contract))
					   :returning 'risk-id)
			    :single!))
		  ;; create a risk-risk-detail with a detail-detail
		  (detail-id (query
			      (:insert-into 'risk-risk-detail
					    :set 'risk-id risk-id
					    'risk-detail-id risk-detail-id
					    :returning 'risk-risk-detail-id)
			      :single!))
		  ;; create the detail detail
		  (detail-detail-id (query
				     (:insert-into 'risk-risk-detail-detail
						   :set 'risk-risk-detail-id detail-id
						   'detail (arnesi:random-string)
						   :returning 'risk-risk-detail-detail-id)
				     :single!)))
	     ;; delete the risk-risk-detail
	     (query (:delete-from 'risk-risk-detail
				  :where (:= 'risk-risk-detail-id
					     detail-id)))
	     (is (null (query (:select '*
				       :from 'risk-risk-detail-detail
				       :where (:= 'risk-risk-detail-id
						  detail-id)))))
	     ;; create a replacement risk-detail (same info)
	     (let* ((detail-id (query
				(:insert-into 'risk-risk-detail
					      :set 'risk-id risk-id
					      'risk-detail-id risk-detail-id
					      :returning 'risk-risk-detail-id)
			      :single!))
		   (detail-detail-id (query
				      (:insert-into 'risk-risk-detail-detail
						    :set 'risk-risk-detail-id detail-id
						    'detail (arnesi:random-string)
						    :returning 'risk-risk-detail-detail-id)
				     :single!)))
	       ;; delete the risk
	       (finishes (query (:delete-from 'risk :where (:= 'risk-id risk-id))))
	       (is (null (query (:select '*
					 :from 'risk-risk-detail
					 :where (:= 'risk-id risk-id)))))
	       (is (null (query (:select '*
					 :from 'risk-risk-detail-detail
					 :where (:= 'risk-risk-detail-id
						    detail-id)))))))))))))

(deftest (risk-risk-detail-unique-constraint :in risk-risk-details) ()
  "Check  enforcement of one risk-detail-detail per risk-detail.risk-detail-type"
  (with-connection *connect-spec*
    (let* ((primary-risk-detail (query (:limit (:order-by (:select '* :from 'risk-detail)
							  (:random))
					       1)
				       :plist))
	   (duplicate-risk-detail (query (:limit
					  (:order-by
					   (:select '* :from 'risk-detail
						    :where (:and (:not (:= 'risk-detail-id
									   (getf primary-risk-detail
										 :risk-detail-id)))
								 (:= `risk-detail-type-id
								     (getf primary-risk-detail
									   :risk-detail-type-id))))
					   
					   (:random))
					  1)
					 :plist))
	   (risk-type-name (query (:select 'risk-type-name :from 'risk-detail-type
					   :where (:= 'risk-detail-type-id
						      (getf primary-risk-detail
							    :risk-detail-type-id)))
				  :single!)))
      (in-transaction
       (lambda ()
	 (let* ((risk-id (query
			  (:insert-into 'risk
					:set 'risk-type-name risk-type-name
					'policy-id (select-one-randomly `(:select 'policy-id :from policy))
					'contract-id (select-one-randomly `(:select 'contract-id :from contract))
					:returning 'risk-id)
			  :single!)))
	   (finishes (query (:insert-into 'risk-risk-detail
					  :set 'risk-id risk-id
					  'risk-detail-id (getf primary-risk-detail :risk-detail-id))))
	   (signals database-error
	     (query (:insert-into 'risk-risk-detail
				  :set 'risk-id risk-id
				  'risk-detail-id (getf duplicate-risk-detail :risk-detail-id))))))))))



(deftest (risk-risk-detail-detail-check-constraint :in risk-risk-details) ()
  (with-connection *connect-spec*
    ;; Ensure that a risk-risk-detail-detail cannot be inserted when
    ;; risk-detail.detail-detail-type IS NULL
    (let* ((risk-detail (query (:limit (:order-by (:select '* :from 'risk-detail
							   :where (:is-null 'detail-detail-type))
						  (:random))
					       1)
				       :plist))
	   (risk-type-name (query (:select 'risk-type-name :from 'risk-detail-type
					   :where (:= 'risk-detail-type-id
						      (getf risk-detail
							    :risk-detail-type-id)))
				  :single!)))

      (in-transaction
       (lambda ()
	 (let* ((risk-id (query
			 (:insert-into 'risk
				       :set 'risk-type-name risk-type-name
				       'policy-id (select-one-randomly `(:select 'policy-id :from policy))
				       'contract-id (select-one-randomly `(:select 'contract-id :from contract))
				       :returning 'risk-id)
			 :single!))
	       (risk-risk-detail-id (query (:insert-into 'risk-risk-detail
							 :set 'risk-id risk-id
							 'risk-detail-id (getf risk-detail :risk-detail-id)
							 :returning 'risk-risk-detail-id)
					   :single!)))
	   (signals database-error
	     (query (:insert-into 'risk-risk-detail-detail
				  :set 'risk-risk-detail-id risk-risk-detail-id
				  'detail "foo")))))))
    

    ;; For each risk-detail where detail-detail-type is not null
    ;;  create a risk of the appropriate type
    ;;  create a risk-risk-detail of the appropriate type

    (dolist (risk-detail (select-objects 'maxclaims::risk-detail
					 :where `(:not (:is-null 'detail-detail-type))))
      (in-transaction
       (lambda ()
	 (let* ((risk-id (query
			  (:insert-into 'risk
					:set 'risk-type-name (maxclaims::risk-type.type-name
							      (maxclaims::risk-detail-type.risk-type
							       (maxclaims::risk-detail.risk-detail-type risk-detail)))
					'policy-id (select-one-randomly `(:select 'policy-id :from policy))
					'contract-id (select-one-randomly `(:select 'contract-id :from contract))
					:returning 'risk-id)
			  :single!))
		(risk-risk-detail-id (query (:insert-into 'risk-risk-detail
							  :set 'risk-id risk-id
							  'risk-detail-id (maxclaims::risk-detail.risk-detail-id risk-detail)
							  :returning 'risk-risk-detail-id)
					    :single!)))
	   ;;  create a risk-risk-detail-detail where:
	   ;;   the syntax is valid (text/timestamp/numeric--throw an error
	   ;;                        for any other types )
	   (finishes
	     (query (:insert-into 'risk-risk-detail-detail
				  :set 'risk-risk-detail-id risk-risk-detail-id
				       'detail (let ((dd (maxclaims::risk-detail.detail-detail-type risk-detail)))
						 (cond ((string= dd "text")
							(arnesi:random-string))
						       ((string= dd "numeric")
							(random 66666))
						       ((string= dd "timestamp")
							(simple-date:encode-timestamp
							 2010 01 01))
						       (t (error "Unhandled detail-detail type")))))))
	   ;;   the syntax is NOT valid (except for text, naturally)
	   (unless (string= (maxclaims::risk-detail.detail-detail-type risk-detail)
			    "text")
	     (signals database-error
	       (query (:insert-into 'risk-risk-detail-detail
				  :set 'risk-risk-detail-id risk-risk-detail-id
				       'detail "not valid"))))))))))
