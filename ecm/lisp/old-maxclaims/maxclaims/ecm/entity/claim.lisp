(defpackage :ecm/entity/claim
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/user #:user #:user-id)
  (:import-from :ecm/database)
  (:export #:claim-diary
	   #:claim-summary
	   #:find-claim
	   #:update-claim
     #:clone-claim
	   #:claim-crawford-number
	   #:find-claim-crux
	   #:list-examiners
	   #:list-claim-causes
	   #:list-industry-codes
	   #:list-line-of-businesses
	   #:list-coverages
	   #:find-cause))
(in-package :ecm/entity/claim)

(defun find-cause (code)
  (ecm/database:query/return-jso 
    (:select (:json_build_object
	      "description" 'claim_cause_type
	      "code" 'cause_code)
	     :FROM 'claim_cause
	     :WHERE (:= 'cause-code code))))

(defun claim-diary (claim-id)
  (ecm/database:query/return-jso
    (:select (:jsi.claim-diary claim-id))))

(defun claim-crawford-number (claim-id)
  (ecm/database:query
    (:select 'crawford_claim_id :FROM 'crawford_claim
	     :where (:= claim-id 'claim-id))
    :single)) 

(defun claim-summary (claim-id)
  (ecm/database:query/return-jso
    (:select (:jsi.claim-summary claim-id))))

(defun find-claim-crux (claim-id)
  (ecm/database:query/return-jso
    (:select (:jsi.claim-crux claim-id))))

(defun find-claim (claim-id)
  (ecm/database:query/return-jso
    (:select (:jsi.claim-summary claim-id))))

(defun update-claim (claim-id &rest set &key &allow-other-keys)
  (ecm/database:query/return-jso
    (s-sql:sql-compile `(:update 'claim :set ,@set
				 :where (:= claim-id ,claim-id)
				                 :returning (:jsi.claim-summary claim-id)))))

(defun clone-claim (claim-id &key contract-id subscription)
  (ecm/database:query/return-jso
	   "
SELECT to_json(clone_claim($1, json_build_object('contract_id', $2::int, 'subscription', $3::numeric)));"
     claim-id contract-id subscription
	   :column))

(defun list-claim-causes ()
 (mapcar #'ecm/json:read-json-from-string
	  (ecm/database:query
	   "
  SELECT json_build_object(
   'description', claim_cause_type,
   'code', cause_code)
    FROM claim_cause"
	   :column)))

(defun list-examiners ()
  (mapcan #'ecm/json:read-json-from-string
	  (ecm/database:query
	   (:order-by (:select
		       (:json-agg (:jsi.corpus-summary 'person-id))
		       :from 'app-adjuster
		       :where 'active
		       :group-by 'person-id)
		      (:person-name 'person-id))
	   :column)))

(defun list-industry-codes ()
  (mapcar #'ecm/json:read-json-from-string
	  (ecm/database:query
	   "SELECT json_build_object(class, codes) FROM( select industry_classification AS class, json_agg(json_build_object('industry', industry,'description', description)) AS codes from ibc_code  GROUP BY industry_classification) as foo;"
	   :column)))

(defun list-line-of-businesses ()
  (ecm/database:query
   "SELECT lob FROM line_of_business;"
   :column))

(defun list-coverages ()
  (ecm/database:query
   "SELECT coverage FROM coverage;"
   :column))
  
  

  
