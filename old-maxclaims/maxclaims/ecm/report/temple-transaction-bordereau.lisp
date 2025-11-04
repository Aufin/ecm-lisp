(defpackage :ecm/report/temple-transaction-bordereau
  (:use :cl)
  (:import-from :ecm/json)
  (:import-from #:ecm/spreadsheet
		#:create-spreadsheet)
  (:export #:temple-transaction-bordereau-spreadsheet
           #:temple-transaction-bordereau))
(in-package :ecm/report/temple-transaction-bordereau)

(defun temple-transaction-bordereau 
    (contract-numbers start-date interval 
     &key (row '(json_agg (to_json transaction-bordereau)))
       (order-by '(:raw "(tb.transaction_bordereau).\"Transaction Authorization Date\"")))
  (postmodern:query 
   (s-sql:sql-compile 
    `(:select
      ,row
      :from (:as 
             (:order-by 
              (:select 
               transaction-bordereau
               :FROM 
               (:as 
                (:select (transaction-bordereau 
                          contract.* ,start-date 
                          (:raw ,(format nil "INTERVAL '~A'"
                                         interval)))
                         :from contract
                         :where (:= contract_id 
                                    (:raw 
                                     ,(format nil "ANY(ARRAY[~A])" 
                                              contract-numbers))))
                tb)
               )
	      ,order-by)
             tb)
      :where (:not (:= (:raw "(tb.transaction_bordereau).\"Heading\"")
		       "TPA"))))
   :single))

(defun temple-transaction-bordereau-spreadsheet (jsos)
  (let* ((list (etypecase jsos
                 (string (ecm/json:read-json-from-string jsos))
                 (list jsos)))
         (alists (mapcar #'ecm/json:jso-alist list))
         (sheet (create-spreadsheet alists
                                    :format-dollarsign nil
				    :calculate-totals nil)))
    sheet))
    
    
    

  
           
                                       
    
    
    

