(defpackage :maxclaims/entry-point/reports
  (:use :cl)
  ;; entry point
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler
		#:redirect)
  (:import-from :maxclaims/entry-point/toplevel
		  #:http-parameters-as-alist)

  ;; web display
  (:import-from :maxclaims/ecm-description/report
		#:select-agency-cheque-register
		#:select-contract-cheque-register)
  (:import-from :maxclaims/report/timecard
		 #:select-timecards)
  (:import-from :maxclaims/ecm-description/report/timecard
		#:make-timecard-report)
  (:import-from :maxclaims/report/claim-authority
		 #:select-claims-over-authority)
  (:import-from :maxclaims/ecm-description/report/claim-authority
		#:make-claim-authority-report)
  (:import-from :maxclaims/web-display/html-page
		#:get-app-user
		#:with-user-html-page)

  (:import-from :maxclaims/web-display/navbar
		#:navbar)
  (:import-from :maxclaims/web-display/display
		#:display)
  (:import-from :maxclaims 
		#:with-udb)
  (:import-from :maxclaims/entry-point/spreadsheet 
		#:spreadsheet-list)

  (:import-from :maxclaims/web-display/report
		#:bordereau-report-page))
  
(in-package :maxclaims/entry-point/reports)

(defun user-can-view-admin? (&optional (user maxclaims::$app-user))
  (getf (maxclaims/data-entity/app-user:app-user-get-acl user "admin_reports")
		:READ NIL))

(defun reports-handler ()
    (let ((reports (http-parameters-as-alist "report")))
    (flet ((geta (value)
	     (cadr (assoc value reports :test #'string=))))      
      (if reports	
	  (let ((cheque-register (ignore-errors 
				   (parse-integer (geta "agency-cheque-register"))))
		(contract-cheque-register (ignore-errors 
				   (parse-integer (geta "contract-cheque-register"))))
		(contract-bordereau (ignore-errors 
				      (parse-integer (geta "contract-bordereau"))))
		(claim-bordereau (ignore-errors 
				   (parse-integer (geta "claim-bordereau"))))
		(inter-hannover (ignore-errors 
				  (parse-integer (geta "inter-hannover-bordereau"))))
		(contracts-bordereau 
		 (ignore-errors 
		   (geta "contracts-bordereau")))
		(agency-bordereau (ignore-errors 
		   (geta "agency-bordereau")))
		(claim-authority 
		 (geta "claim-authority"))
		(timecard
		 (geta "timecard")))
	    (cond (timecard 
		   
		   (let ((ca (maxclaims::with-udb 
			       (make-timecard-report 
				(select-timecards)))))
		     (maxclaims/web-display/view:view-page 
		      ca :tabs-inline t)))
		  (claim-authority 
		   
		   (let ((ca (maxclaims::with-udb 
			       (make-claim-authority-report 
				(select-claims-over-authority)))))
		     (maxclaims/web-display/view:view-page 
		      ca :tabs-inline t)))
		     
		   (cheque-register 
		   (let ((cheque-register 
			  (maxclaims::with-udb 
			    (select-agency-cheque-register 
			     cheque-register
			     :start-date (geta "start-date")
			     :end-date (geta "end-date")
			     :risk-type (let ((r (geta "risk-type")))
					  (unless (string-equal r "NULL")
					    r))))))
;		     (break "~A" cheque-register)
		     (maxclaims/web-display/view:view-page 
		      cheque-register :tabs-inline t)))
		  (contract-cheque-register 
		   (let ((cheque-register
			  (maxclaims::with-udb 
			    (select-contract-cheque-register
			     contract-cheque-register			     
			     :start-date (geta "start-date")
			     :end-date (geta "end-date")
			     :risk-type (let ((r (geta "risk-type")))
					  (unless (string-equal r "NULL")
					    r)))
			    )))

		     (maxclaims/web-display/view:view-page 
		      cheque-register
		      :tabs-inline t)))
		  (contract-bordereau 
		   (let ((bordereau
			  (maxclaims::with-udb 
			    (maxclaims/report/bordereau:find-bordereau
			     contract-bordereau
			     :start-date (geta "start-date")
			     :end-date (geta "end-date")
			     :risk-type (let ((r (geta "risk-type")))
					  (unless (string-equal r "NULL")
					    r))))))		     
		     (with-udb 
		       (bordereau-report-page
			bordereau
			:line-fn #'rest
			:object (make-instance '
				 maxclaims/ecm-description::contract-bordereau
				 :contract contract-bordereau
				 :start-date (geta "start-date")
				 :end-date (geta "end-date")
				 :risk-type (let ((r (geta "risk-type")))
					      (unless (string-equal r "NULL")
						r)))))))
		  (claim-bordereau 
		   (let ((bordereau
			  (maxclaims::with-udb 
			    (maxclaims/report/bordereau:find-bordereau
			     claim-bordereau
			     :fields (maxclaims/report/bordereau::claim-bordereau-fields)
			     :start-date (geta "start-date")
			     :end-date (geta "end-date")
			     :risk-type (let ((r (geta "risk-type")))
					  (unless (string-equal r "NULL")
					    r)))))) 
		     (with-udb 
		       (bordereau-report-page
			bordereau
			:line-fn #'rest
			:object (make-instance '
				 maxclaims/ecm-description::claim-bordereau
				 :contract claim-bordereau
				 :start-date (geta "start-date")
				 :end-date (geta "end-date")
				 :risk-type (let ((r (geta "risk-type")))
					      (unless (string-equal r "NULL")
						r)))))))

		  (inter-hannover
		   (let ((bordereau
			  (maxclaims::with-udb 
			    (maxclaims/report/bordereau:find-bordereau
			     inter-hannover
			     :fields (maxclaims/report/bordereau::inter-hannover-bordereau-fields)
			     :start-date (geta "start-date")
			     :end-date (geta "end-date")
			     :risk-type (let ((r (geta "risk-type")))
					  (unless (string-equal r "NULL")
					    r)))))) 
		     (with-udb 
		       (bordereau-report-page
			bordereau
			:line-fn #'rest
			:object (make-instance 'maxclaims/ecm-description::inter-hannover-bordereau
				 :contract inter-hannover
				 :start-date (geta "start-date")
				 :end-date (geta "end-date")
				 :risk-type (let ((r (geta "risk-type")))
					      (unless (string-equal r "NULL")
						r)))))))
		  (agency-bordereau 
		   (agency-bordereau-handler 
		    agency-bordereau
		    :start-date (geta "start-date")
		    :end-date (geta "end-date")
		    :risk-type (let ((r (geta "risk-type")))
				 (unless (string-equal r "NULL")
				   r))
		    )
		   )
		  (contracts-bordereau 
		   (let ((bordereau
			  (maxclaims::with-udb 
			    
			    (maxclaims/report/bordereau:find-bordereau/payee
			     (loop :for cid 
			       :in (split-sequence:split-sequence 
				    #\, contracts-bordereau
				    :remove-empty-subseqs t)
			       :collect (parse-integer cid)) 
			     :start-date (geta "start-date")
			     :end-date (geta "end-date")
			     :risk-type (let ((r (geta "risk-type")))
					  (unless (string-equal r "NULL")
					    r)))
			    )))
		     #+ (or) (break "~A" bordereau)
		     (bordereau-report-page
		      bordereau
		      :object 
		      (make-instance 
		       'maxclaims/ecm-description::contracts-bordereau
		       :report bordereau
		       :start-date (geta "start-date")
		       :end-date (geta "end-date")
		       :risk-type (let ((r (geta "risk-type")))
				    (unless (string-equal r "NULL")
				      r)))
		      )))))

	  (with-user-html-page (:title "ECM Reports")
	    (navbar :active "Reports")
	    (<:div 
	     :class "container" 
             (<:h1 "New Reports")
	     (<:ul
	      (<:li 
	       (<:a :href "/ecm/report/time-recorded"
		    :target "_blank"
		    "Your Hours Recorded"))
	      (<:li 
	       (<:a :href "/ecm/report/temple-transaction-bordereau"
		    :target "_blank"
		    "Temple Transactional Bordereau"))
	      (<:li 
	       (<:a :href "/ecm/report/pillar-3"
		    :target "_blank"
		    "Pillar 3"))
	      (<:li 
	       (<:a :href "/ecm/report/mi"
		    :target "_blank"
		    "Performance Management Information (MI)")))

		 
	     (when (user-can-view-admin?)

                    
	       (<:h1 "Administrator Reportsa")
	       (<:li 
		(<:a :href "create?create[type]=interim-report&access[read-only]=false"
		     :target "_blank"
		     "Interim Amounts"))
	       (<:li 
		(<:a :href "create?create[type]=open-claim-report&access[read-only]=false"
		     :target "_blank"
		     "Open Claims"))
	       
	       (<:li 
		(<:a :href "create?create[type]=time-recorded-report&access[read-only]=false"
		     :target "_blank"
		     "Timecards: Time Recorded"))
	       (<:li 
		(<:a :href "/ecm/reports?report[claim-authority]=claim-authority-report&access[read-only]=false"
		      :target "_blank"
		      "Claims over Authority"))
		(<:li 
		 (<:a :href "/ecm/reports?report[timecard]=timecard-report&access[read-only]=false"
		      :target "_blank"
		      "Timecards for October")))
	     (<:h1 "Reports")
	     (<:ul 
	      (<:li 
	       (<:a :href "/ecm/cheque-register/agency"
		    :target "_blank"
		    "Agency Cheque Register"))
	      (<:li 
	       (<:a :href "/ecm/cheque-register/contract"
		    :target "_blank"
		    "Contract Cheque Register"))
	      (<:li 
	      (<:a :href "create?create[type]=contract-bordereau&access[read-only]=false"
		    :target "_blank"
		    "Casualty/Liability Claims Bordereau"))
	      (<:li 
	       (<:a :href "create?create[type]=claim-bordereau&access[read-only]=false"
		    :target "_blank"
		    "Property Claims Bordereau"))
	      (<:li 
	       (<:a :href "create?create[type]=inter-hannover-bordereau&access[read-only]=false"
		    :target "_blank"
		    "Inter-Hannover Bordereau"))
	      (<:li 
	       (<:a :href "/ecm/report/agency-bordereau"
		    :target "_blank"
		    "Agency Bordereau"))
	      (<:li 
		(<:a :href "create-report?create[type]=contracts-bordereau&access[read-only]=false"
		     :target "_blank"
		     "Numerous Contract Bordereau with Payee Amount")))
	     (spreadsheet-list)))))))

(defun contract-bordereau-excel-file (string)
  (let* ((reports (http-parameters-as-alist "report"))
	 (contract (let ((id (or  (cadr (assoc "contract-bordereau" reports :test #'string=))
				  (cadr (assoc "claim-bordereau" reports :test #'string=)))))
		     (when id (maxclaims::find-object 'maxclaims::contract id))))
	 (name (if (not contract) 
		   (error "No contract for contract-bordereau-excel-file")
		   (cl-ppcre:regex-replace-all
		    "/| " 
		    (format nil "~A-~A-~A-~A" 
			    (maxclaims::contract.contract-number contract)
			    (ignore-errors  (multiple-value-bind (year month day) 
						(simple-date:decode-date 
						 (maxclaims::contract.effective-date contract))
					      (format nil "~2,'0d-~2,'0d-~4,'0d"
						      day month year)))
			    (ignore-errors (maxclaims::contract.agency-id contract))
			    (local-time:format-timestring
			     nil 
			     (local-time:universal-to-timestamp (get-universal-time)) 
			     :format '(:year "-" :month "-" 
				       :day "T" :hour ":" 
				       :min ":" :sec)))
		    "---")))
	 (html-file (format nil "/tmp/~A-contract-bordereau.html" name))
	 (xls-file (format nil "/tmp/~A-contract-bordereau.gnumeric" name)))
    
    (alexandria:with-output-to-file (s html-file
				       :if-exists :overwrite
				       :if-does-not-exist :create)
      (princ string s))
    
    (sb-ext:run-program "/usr/bin/ssconvert" `(,html-file
					       ,xls-file))

    
    (values xls-file name)))

(define-easy-handler (reports :uri "/ecm/reports")
    ((as-excel :real-name "report[as-excel]"))
  (hunchentoot:redirect "/ecm/report"
			:protocol
			(eval (read-from-string "ecm/hunchentoot:*protocol*"))))

  
  
