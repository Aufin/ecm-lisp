(in-package :maxclaims)

(defcomponent cheque-register-date-range-component (date-range-component)
  ((start-date :initarg 
	       :start-date 
	       :accessor start-date
	       :input (:type simple-date))
   (end-date :initarg 
	     :end-date 
	     :accessor end-date
	     :input (:type simple-date))
   (agency :accessor agency 
	   :initarg :agency
	   :deactivate (editable)))
  (:metaclass described-component-class))

(define-description cheque-register-date-range-component 
    (description-for-cheque-register-date-range-component)
  ((active-attributes :value '(agency start-date end-date))))
  
(defcomponent cheque-register-report-viewer (report-viewer)
  ())

(defmethod render-html-body ((self cheque-register-report-viewer))
  (with-db 
    (<:h1 "Cheque Register")
    (<:div
     (with-slots (report-component) self
       (display self report-component :attributes 
		'((agency :deactivate (link-to-viewer)) start-date end-date)) ))
    (<:br :style "clear:both")
    (let ((results (results self))) 
      (dolist (r results)
	(let ((report (rest r)))
	  (when (not (endp (rest report)))

	    (<:h2 (<:as-html (risk-type.type-name (first r))))
	    (render-report-viewer self (rest r))))))))


(defaction view-cheque-register-report (agency)
  (when agency 
    (let ((value (call 'cheque-register-date-range-component :agency agency)))
      (with-slots (start-date end-date agency) value
	(call-component 
	 $window 
	 (make-instance 'cheque-register-report-viewer 
			:results 
			(mapcar (lambda (type)
				  (cons type (cons
					      (list "Payee" "Claim Number" "Policy" 
						    "Insured" "Date of Loss" "Amount" 
						    "Contract" "Cheque Number" "Status" 
						    "Cheque Date" "Expense Type")
					      (mapcar (lambda (s)
					; (setf (sixth s) (format nil "$~$" (sixth s)))
							s)
						      (find-cheque-register agency start-date end-date type)))))
				(risk-types))
			:report-component value))))))


(defun find-cheque-register (agency start-date end-date risk-type)
  (sort (let ((agency-id (person.person-id agency))
	      (rofl::*row-reader*' postmodern::list-row-reader))
	  (with-db 
	      (select '(:as (:|\|\|| 
			     (:coalesce payee.first-name "") 
			     " " 
			     (:coalesce payee.last-name "") 
			     " " 
			     (:coalesce payee.company-name "")) payee)
		      'claim-transaction.claim-id
		      'policy-number 
		      '(:as  (:|\|\|| 
			      (:coalesce  insured.first-name "") 
			      " " 			  
			      (:coalesce  insured.last-name "") 
			      " " 
			      (:coalesce insured.company-name "")) 
			insured) 
		      'claim.date-of-loss
		      'claim-transaction.amount 
		      'contract-number
		      'cheque-number
		      '(claim-status claim.claim-id)
		      'claim-transaction.transaction-date
		      'claim-transaction.expense-type
		      :from 'claim-transaction 
		      :inner-join 'claim :on '(:= 
					       claim-transaction.claim-id
					       claim.claim-id)
		      :inner-join 'risk
		      :on `(:= claim.risk-id risk.risk-id) 
		      :inner-join 'contract :on '(:= risk.contract-id 
						  contract.contract-id)
		      :inner-join 'policy :on  '(:= risk.policy-id 
						 policy.policy-id)
		      :inner-join '(:as person payee) 
		      :on '(:= claim-transaction.payee-id payee.person-id)
		      :left-join '(:as person insured) :on '(:= policy.insured-id insured.person-id)		   
		      :where `(:and (:or
				     ;; fixme: hardcodes type ids!
				     (:= transaction-type-id 3)
				     (:= transaction-type-id 4)
				     (:= transaction-type-id 5))
				    (:= contract.agency-id ,agency-id)
				    (:= risk-type-name ,(risk-type.type-name risk-type))
				    (:between transaction-date ,start-date ,end-date))
		      ))) #'< :key 'second))

