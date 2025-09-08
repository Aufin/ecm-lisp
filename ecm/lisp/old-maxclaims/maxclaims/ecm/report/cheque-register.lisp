(defpackage :ecm/report/cheque-register
  (:use :cl)
  (:import-from :ecm/spreadsheet
		#:cell-value)
  (:export #:find-agency-cheque-register
	   #:find-contract-cheque-register))
(in-package :ecm/report/cheque-register)

(defparameter *cheque-register-headings*
  '(
    ("Payee" (:raw "trim(both ' ' from concat_ws(' ', 
                pay.first_name, pay.last_name, pay.company_name))"))
    ("Claim Number" c.claim-id)
    ("Policy" p.policy-number)
    ("Insured" (:raw "trim(both ' ' from concat_ws(' ', 
                  i.first_name, i.last_name, i.company_name))"))
    ("Date of Loss" c.date-of-loss)
    ("Amount" (:\|\| "$" ct.amount))
    ("Contract" co.contract-number)
    ("Reference Number" ct.reference_number)
    ("Cheque Number" ct.cheque-number)
    ("Status" (claim-status c.claim-id))
    ("Cheque Date" ct.transaction-date)
    ("Expense Type" (:coalesce ct.expense-type ""))))

(setf maxclaims/report/cheque-register::*cheque-register-headings*
     *cheque-register-headings*)

(defstruct (cheque-register-heading (:type list))
  name s-sql)

(defun heading-as (heading)
  `(:as ,(cheque-register-heading-s-sql heading)
	(:raw ,(concatenate 'string "\""
		      (cheque-register-heading-name heading)
		      "\""))))

(defparameter *cheque-register-transaction-types*
  '(3 4 5)
  ";; 3 | Cheque - Loss
;; 4 | Cheque - Expense
;; 5 | Cheque - In House Payout")

(defun select-cheque-register (where)
  (let ((c (postmodern:query
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
	    :str-alists)))
    ;; (break "~A" c)
    c))


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
          (select-cheque-register 
       `(:and ,where
	      (:in  ct.transaction-type-id 
		    (:set ,@*cheque-register-transaction-types*))
	      (:between transaction-date ,start-date ,end-date)
	      ,@(when risk-type 
		  `((:= risk-type-name ,risk-type))))))
	    
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
   (maxclaims/ecm-description/report:find-cheque-register 
    agency-id
    :start-date start-date 
    :end-date end-date
    :risk-type risk-type))
