(defpackage :maxclaims/report/cheque-register
  (:use :cl)
  (:import-from :rofl 
		#:select)
  (:import-from :maxclaims
		  #:with-adb
		  #:risk-type.type-name)
  (:export #:find-cheque-register
	   #:find-contract-cheque-register))

(in-package :maxclaims/report/cheque-register)

(defparameter *cheque-register-headings*
  '());; Set from ecm/report/cheque-register

(defstruct (cheque-register-heading (:type list))
  name s-sql)

(defun heading-as (heading)
  `(:as ,(cheque-register-heading-s-sql heading)
	(:raw ,(concatenate 'string "\""
		      (cheque-register-heading-name heading)
		      "\""))))

(defparameter *cheque-register-transaction-types*
  ;; 3 | Cheque - Loss
  ;; 4 | Cheque - Expense
  ;; 5 | Cheque - In House Payout

  '(3 4 5))
	
    

#+(or)(list  "Claim Number" "Policy" 
       "Insured" "Date of Loss" "Amount" 
       "Contract" "Cheque Number" "Status" 
       "Cheque Date" "Expense Type")

(defun select-cheque-register (where)
  (postmodern:query
   (s-sql:sql-compile 
    `(:order-by 
      (:select 
       ,@(mapcar #'heading-as *cheque-register-headings*)
       :from (:as claim-transaction ct)
       :inner-join (:as claim-transaction-type type)
       :on (:= ct.transaction-type-id type.claim-transaction-type-id)
       :inner-join (:as claim c)
       :on (:= ct.claim-id c.claim-id) 
       :left-join (:as risk r)
       :on (:= c.risk-id r.risk-id)
       :left-join (:as policy  p) 
       :on  (:= r.policy-id 
		p.policy-id)
       :left-join (:as contract co)
       :on (:= r.contract-id 
	       co.contract-id)
       :left-join (:as person i) 
       :on (:= p.insured-id i.person-id)
       :left-join (:as person pay)
       :on (:= ct.payee-id pay.person-id)
       :where ,where)
      'co.contract-number 'c.claim-id 'ct.transaction-date))
   :str-alists))


(defgeneric find-cheque-register (where &key 
					  start-date 
					  end-date
					  risk-type)
  (:method ((where list)
	    &key 
	      (start-date "2010-01-01")
	      (end-date "2012-12-31")
	      risk-type)
    (assert (or (not risk-type) (stringp risk-type)))
    (when (equalp risk-type "")
      (setf risk-type nil))
    (with-adb 
      (select-cheque-register 
       `(:and ,where
	      (:in  ct.transaction-type-id 
		    (:set ,@*cheque-register-transaction-types*))
	      (:between transaction-date ,start-date ,end-date)
	      ,@(when risk-type 
		      `((:= risk-type-name ,risk-type)))))))
	    
  (:method ((agency-id integer) 
	    &key 
	      (start-date "2012-01-01")
	      (end-date "2012-12-31")
	      risk-type)
    (assert (or (not risk-type) (stringp risk-type)))
    (when (equalp risk-type "")
      (setf risk-type nil))
          (select-cheque-register 
	  `(:and (:= co.agency-id ,agency-id)
		 (:in  ct.transaction-type-id 
		       (:set ,@*cheque-register-transaction-types*))
		 (:between transaction-date ,start-date ,end-date)
		 ,@(when risk-type 
			 `((:= risk-type-name ,risk-type)))))))

(defun find-contract-cheque-register (contract-id
				      &rest args)
  (apply #'find-cheque-register 
		     `(:= co.contract-id
			  ,contract-id)
		     args))

(defun find-agency-cheque-register 
    (agency-id  
     &key 
       (start-date "2012-01-01")
       (end-date "2012-12-31")
       risk-type)
  (funcall (read-from-string "MAXCLAIMS/ECM-DESCRIPTION/REPORT:FIND-CHEQUE-REGISTER")
	   agency-id
	   :start-date start-date 
	   :end-date end-date
	   :risk-type risk-type))
  

