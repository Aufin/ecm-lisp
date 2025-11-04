(defpackage :ecm/entity/contract
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/database)
  (:export #:list-contracts
	   #:find-contract))
(in-package :ecm/entity/contract)

(defun list-contracts ()
  (ecm/database:query/return-jso
    "select json_agg(jsi.contract_summary(c.*))
 FROM (SELECT * FROM contract
       ORDER BY effective_date DESC NULLS LAST, expiry_date DESC NULLS LAST, contract_number) 
  AS c"))

(defun find-contract (contract-id)
  (when contract-id 
    (ecm/database:query/return-jso
      (:select (:jsi.contract-summary 'contract.*)
	       :from 'contract
	       :where (:= 'contract-id contract-id)))))
  
