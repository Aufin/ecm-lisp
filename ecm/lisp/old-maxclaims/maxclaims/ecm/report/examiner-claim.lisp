(defpackage :ecm/report/examiner-claim
  (:use :cl)
  (:import-from :ecm/json)
  (:import-from #:ecm/spreadsheet
		#:create-spreadsheet)
  (:export #:examiner-claims-spreadsheet
	   #:examiner-claims))
(in-package :ecm/report/examiner-claim)

(defun examiner-claims (examiner-id)
  (postmodern:query 
   (format nil "SELECT claim_id, date_of_loss::date, status, contract_number, policy_number, person_name(insured_id) AS insured
 FROM claim LEFT JOIN risk USING (risk_id) 
  LEFT JOIN contract USING (contract_id) 
  LEFT JOIN policy USING (policy_id)
 WHERE adjuster_id = ~A 
 ORDER BY claim_id; "
	   examiner-id)
   :str-alists))

(defun examiner-claims-spreadsheet (alists)
  (create-spreadsheet alists
		      :format-dollarsign nil
		      :calculate-totals nil))
    
    
    

  
           
                                       
    
    
    

