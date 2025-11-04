;;; Right now the maxclaims package is way way way to much in one spot
(in-package :maxclaims)

(defaction view-contract-bordereau-report ((contract contract))
  (let ((value (call 'bordereau-date-range-component :contract contract)))
    (with-slots (start-date end-date contract risk-type) value
      (call-component 
       $window 
       (make-instance 
	'bordereau-report-viewer 
	:results 
	(mapcar 
	 (lambda (type) 
	   `(,type 
	     ,(maxclaims.org/report/contract-bordereau:contract-bordereau-report-labels)
	     ,@(contract-bordereau-report 
		(contract.contract-number contract) start-date end-date type)))
	 (if risk-type 
	     (list risk-type) 			   
	     (risk-types)))
	:report-component value)))))

(defun contract-bordereau-report-totals (contract-number start-date end-date type)
  (with-db
    (query 
     (sql-compile 
      `(:select 
	,@(maxclaims.org/report/contract-bordereau:contract-bordereau-report-total-columns
	   start-date end-date)	  
	:from (:as reports.contract-bordereau-claims risk)
	:where (:and
		(:= ,contract-number risk.contract-number)
		(:= ,(risk-type.type-name type) risk.type))))
     :row)))

(defun contract-bordereau-report (contract-number start-date end-date type)
  (cons 
   (contract-bordereau-report-totals contract-number start-date end-date type)
   (with-db
     (query 
      (sql-compile
       `(:order-by
	 (:select
	  ,@(maxclaims.org/report/contract-bordereau:contract-bordereau-report-columns
	     start-date end-date)
	  :from (:as reports.contract-bordereau-claims
		     risk)
	  :where (:and (:= ,contract-number contract-number)
		       (:= ,(risk-type.type-name type) type)
		       (:not 
			(:and 
			 (:= 0 (claim-cheque-expense 
				risk.claim-id ,end-date ,start-date))
			 (:= 0 (claim-loss risk.claim_id ,end-date ,start-date))
			 (:= 0 (claim-incurred risk.claim-id ,end-date))))))
	 risk.claim-id))))))




;;; DELETE ME!
;;; the stuff under here should be deleted if things work


;;; fixme: this is really ugly and requires keeping *three* different
;;; functions in sync with each other.

;;; this is what happens when the client goes to the UK for several
;;; weeks without explaining exactly what they mean and then return a
;;; few days before the hard deadline...

(defvar *bdx-headings* '("Adjusting" "TPA" "Legal" "Indemnity" "Expert Expense")
  "Not all headings are needed in the 'new' contract bordereau. For
  now, stash the list in a variable... in the future let the user
  configure it.")

(defun bdx-by-heading (claim-info-fn claim-id-field end-date &optional start-date)
  "Splice output into s-sql SELECT list"
  (mapcar (lambda (heading)
	    `(,claim-info-fn #+nil,(claim-transaction-heading.name heading)
			     ,heading
			     ,claim-id-field ,end-date ,@(when start-date `(,start-date))))
	  #+nil(select-objects 'claim-transaction-heading)
	  *bdx-headings*))

(defun bdx-label-by-heading (label)
  (mapcar (lambda (heading)
	    (format nil "~A ~A" label heading))
	  *bdx-headings*))

(defun change-in-reserve-exp (claim-id-col end-date start-date)
  `(:- (claim-reserve ,claim-id-col ,end-date)
       (claim-reserve ,claim-id-col ,(simple-date:time-subtract start-date
						  (simple-date:encode-interval :day 1)))))

(defaction view-bordereau-report/heading ((contract contract))
  (let ((value (call 'bordereau-date-range-component :contract contract)))
    (with-slots (start-date end-date contract risk-type) value
      (call-component 
       $window 
       (make-instance 'bordereau-report-viewer 
		      :results 
		      (mapcar 
		       (lambda (type) 
			 (cons type 
			       (cons 
				`("Claim" "Policy" "Broker" 
				  "Insured" "Province" "Date of Loss"
				  "Effective" "Expiry"
					   
				  ,@(bdx-label-by-heading "Paid This Period")
				  "Paid This Period Total"
				  
				  ,@(bdx-label-by-heading "Total Paid")
				  "Total Paid"

				  #+nil"Total Expert Expense"
				  
				  ,@(bdx-label-by-heading "Outstanding Reserve")
				  "Outstanding Reserve"
				  
				  "Total Incurred"
				  "Deductible"
				  "Recovered Deductible"
				  "Recovered Deductible This Period"
				  "Outstanding Deductible"
				  "Change In Reserve This Period"
				  "Status")
				(find-bordereau/heading (contract.contract-number contract)
							start-date
							end-date type))))
		       (if risk-type 
			   (list risk-type) 
			   
			   (risk-types)))
		      :report-component value)))))

(defun find-bordereau-totals/heading (contract-number start-date end-date type)
  (flet ((sum/h (f &optional (period-p t))
	   (mapcar (lambda (hf) `(:\|\| "$" (:sum ,hf)))
		   (bdx-by-heading f 'risk.claim-id end-date (when period-p
							       start-date)))))
    (with-db
      (query (sql-compile `(:select  
			    `"TOTALS" ; claim
			    "" ; policy
			    "" ; broker
			    "" ; insured
			    "" ; province
			    "" ; date of loss
			    "" ; effective
			    "" ; expiry
			    ;; paid this period
			    ,@(sum/h 'claim-paid)
			    (:\|\| "$" (:sum (claim-paid risk.claim_id ,end-date ,start-date)))
			    
			    ;; total paid
			    ,@(sum/h 'claim-paid nil)
			    (:\|\| "$" (:sum (claim-paid risk.claim_id ,end-date)))

			    ;; total expense
			    #+nil(:\|\| "$" (:sum (claim-cheque-expense risk.claim_id ,end-date)))

			    ;; outstanding reserve
			    ,@(sum/h 'claim-outstanding-reserve nil)
			    (:\|\| "$" (:sum (claim-outstanding-reserve risk.claim_id ,end-date)))

			    ;; incurred
			    (:\|\| "$" (:sum (claim-incurred risk.claim_id ,end-date)))
			    
			    "" ; deductible
			    "" ; recovered deductible
			    "" ; outstanding deductible
			    (:\|\| "$" (:sum ,(change-in-reserve-exp
						'risk.claim-id
						end-date start-date)))

			    "" ; status
			    :from (:as reports.contract-bordereau-claims risk)
			    :where (:and
				    (:= ,contract-number risk.contract-number)
				    (:= ,(risk-type.type-name type) risk.type))))
	     :row))))


(defun find-bordereau/heading (contract-number start-date end-date type)
  (cons 
   (find-bordereau-totals/heading contract-number start-date end-date type)
   (flet ((by/h (f &optional (period-p t))
	    (mapcar (lambda (hf) `(:\|\| "$" ,hf))
		   (bdx-by-heading f 'risk.claim-id end-date (when period-p
							       start-date)))))
     (with-db
       (query (sql-compile`(:order-by
			    (:select
			     'risk.claim-id
			     'policy-number
			     'broker-name
			     'insured-name
			     'province
			     'date-of-loss
			     'effective-date
			     'expiry-date

			     ;; paid this period
			     ,@(by/h 'claim-paid)
			     (:\|\|  "$" (claim-paid risk.claim_id ,end-date ,start-date))

			     ;; total paid
			     ,@(by/h 'claim-paid nil)
			     (:\|\|  "$" (claim-paid risk.claim_id ,end-date))

			     ;; total expense
			     #+nil(:\|\|  "$" (claim-cheque-expense risk.claim_id ,end-date))

			     ;; Outstanding Reserve
			     ,@(by/h 'claim-outstanding-reserve nil)
			     (:\|\|  "$" (claim-outstanding-reserve risk.claim_id ,end-date))

			     ;; Incurred
			     (:\|\|  "$" (claim-incurred risk.claim_id ,end-date))
			     
			     (:\|\| "$" deductible)
			     (:\|\| "$" (claim-recovered-deductible risk.claim_id ,end-date))
			     (:\|\| "$" (claim-recovered-deductible risk.claim_id ,end-date, start-date))
			     (:\|\| "$" (claim-deductible risk.claim_id ,end-date))
			     

			     (:\|\| "$" ,(change-in-reserve-exp 'risk.claim-id end-date start-date))

			     'risk.status

			     :from (:as reports.contract-bordereau-claims
					risk)
			     :where (:and (:= ,contract-number contract-number)
					  (:= ,(risk-type.type-name type) type)
					  (:not (:and (:= 0 (claim-cheque-expense risk.claim-id ,end-date ,start-date))
						      (:= 0 (claim-loss risk.claim_id ,end-date ,start-date))
						      (:= 0 (claim-incurred risk.claim-id ,end-date))))))
			    risk.claim-id)))))))

