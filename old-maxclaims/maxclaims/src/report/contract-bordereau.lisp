(defpackage :maxclaims.org/report/contract-bordereau
  (:use :cl)
  (:export #:contract-bordereau-report-columns
	   #:contract-bordereau-report-labels
	   #:contract-bordereau-report-total-columns))

(in-package :maxclaims.org/report/contract-bordereau)

;;; Contract/Claims Bordereau

;;; This will eventually replace the other 'bordereau'  

;;; What we first have is *BORDEREAU-HEADINGS* Right now it is used as
;;; is, but where is used it can be LET bound.

;;; HEADING: The claim TRANSACTION-HEADING.NAME
;;; COLUMN: basically the s-sql that will end up in the SELECT
;;; LABEL: used by VIEW-BORDEREAU-REPORT/HEADING
;;; FIELD: right now a (label column totals)

(defvar *bordereau-headings* '("Adjusting" 
			       "TPA" 
			       "Legal" 
			       "Indemnity" 
			       "Expert Expense"))
  
(defun contract-bordereau-fields-by-label (label s-sql)
  (mapcar (lambda (heading) 
	    (let ((s-sql-fn `(,(first s-sql)
			       ,heading
			       ,@(rest s-sql))))
	      `(,(format nil "~A ~A" label heading)
		 (:\|\| "$" ,s-sql-fn)
		 (:\|\| "$" (:sum ,s-sql-fn)))))
	    *bordereau-headings*))

(defparameter *contract-bordereau-report* 
  (flet ((givr (s-sql)
	   `((:\|\| "$" ,s-sql)
	     (:\|\| "$" (:sum ,s-sql)))))
    `(("Claim" claim-id "TOTALS")
      ("Policy" policy-number "")
      ("Broker" broker-name "")
      ("Insured" insured-name "")
      ("Province" province "")
      ("Date of Loss" date-of-loss "")
      ("Effective" effective-date "")
      ("Expiry" expiry-date "")
      ,@(contract-bordereau-fields-by-label
	 "Paid This Period" '(claim-paid claim-id end-date start-date))
      ("Paid This Period Total"
       ,@(givr '(claim-paid risk.claim-id end-date start-date)))  
      ,@(contract-bordereau-fields-by-label
	 "Total Paid" '(claim-paid claim-id end-date))
      ("Total Paid"
       ,@(givr '(claim-paid risk.claim-id end-date)))
      #+nil("Total Expert Expense"  
	    (:\|\|  "$" (:claim-cheque-expense risk.claim_id ,end-date)))
      ,@(contract-bordereau-fields-by-label
	 "Outstanding Reserve" 
	 '(claim-outstanding-reserve claim-id end-date))
      ("Outstanding Reserve"
       ,@(givr '(claim-outstanding-reserve risk.claim-id end-date)))
      ("Total Incurred"
       ,@(givr '(claim-incurred risk.claim-id end-date)))
      ("Deductible to be Recovered"
       ,@(givr 'deductible))
      ("Recovered Deductible"
       ,@(givr '(claim-recovered-deductible risk.claim-id end-date start-date)))
      ("Recovered Deductible This Period"
       ,@(givr '(claim-recovered-deductible risk.claim-id end-date)))
      ("Outstanding Deductible"
       ,@(givr '(claim-deductible risk.claim-id end-date)))
      ("Change In Reserve This Period"
       ,@(givr `(:- (claim-reserve risk.claim-id end-date)
		    (claim-reserve risk.claim-id 
				   (:- start-date
				       ,(simple-date:encode-interval :day 1))))))
      ("Deductible" ,@(givr 'total-deductible))
      ("Status" risk.status))))

(defun contract-bordereau-report-labels ()
  (mapcar #'car *contract-bordereau-report*))

(defun %contract-bordereau-report-columns (start-date end-date
					   &optional (s-sql-loc #'second))
  (labels ((rep (rep)
	     (typecase rep
	       (list (loop :for v :in rep :collect (rep v)))
	       (symbol (if (find rep '(start-date end-date))
			   (eval `(let ((start-date ,start-date)
					(end-date ,end-date))
				    (declare (ignorable start-date end-date))
				    ,rep))
			   rep))
	       (t rep))))
    
    (loop :for f :in *contract-bordereau-report*
       :collect (rep (funcall s-sql-loc f)))))

(defun contract-bordereau-report-columns (start-date end-date)
  (%contract-bordereau-report-columns start-date end-date))

(defun contract-bordereau-report-total-columns (start-date end-date)
  (%contract-bordereau-report-columns start-date end-date #'third))
  

