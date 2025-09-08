(in-package :maxclaims)

(defcomponent report-window-download (ucw:standard-window-component)
  ((name :accessor name :initarg :name)
   (xls-name :accessor xls-name :initarg :xls-name)))

(defmethod render-html-body ((self report-window-download))
  (<ucw:a :action (download-contract-bordereau-excel (name self)
						     (xls-name self))
	     (<:as-html "download as excel" )))

(defun find-bordereau/excel (contract-number start-date end-date type)
  (let ((rofl::*row-reader*' postmodern::list-row-reader))
    (cons 
     (car (find-bordereau-totals contract-number start-date end-date type
				 :number-of-empty-columns 11))
     (rofl::%query `(:order-by
		       (:select  
			'policy-number
			'branch-name
			'underwriter-name
			'broker-name
			'risk.claim-id
			'risk.adjuster-name
			'insured_name 
			'province
			'date-of-loss
			(:\|\| "$" (claim-paid "TPA" risk.claim-id ))
			(:\|\| "$" (claim-paid "Expert Expense" risk.claim-id ))
			'effective-date
			(:as (:\|\|  "$" (claim-loss risk.claim_id ,end-date ,start-date))
			     loss)
			(:as (:\|\| "$" (claim-cheque-expense risk.claim-id ,end-date ,start-date)) expense)
			(:\|\| "$" (claim-loss risk.claim-id ,end-date))
			(:\|\| "$" (claim-cheque-expense risk.claim-id ,end-date))
			(:\|\| "$" (claim-open-reserve risk.claim-id ,end-date))
			(:\|\| "$" (claim-outstanding-reserve risk.claim-id ,end-date))
			(:\|\| "$" (claim-paid risk.claim-id ,end-date))
			(:\|\| "$" (claim-incurred risk.claim-id ,end-date))
			'status
			:from (:as reports.contract-bordereau-claims risk)
			:where (:and (:= ,contract-number contract-number)
				     (:= ,(risk-type.type-name type) type)
				     (:not (:and (:= 0 (claim-cheque-expense risk.claim-id ,end-date ,start-date))
						 (:= 0 (claim-loss risk.claim_id ,end-date ,start-date))
						 (:= 0 (claim-incurred risk.claim-id ,end-date))))))
		       risk.claim-id)))))

(defun find-claims-bordereau (start-date end-date contract risk-type)
  (mapcar 
   (lambda (type) 
     (cons type 
	   (cons 
	    (list "Policy" "Branch" "Underwriter" "Broker" "Claim" "Adjuster"
		  "Insured" "Province" "Date of Loss" "TPA"  
		  "Expert Expense" "Effective" "Loss this period" 
		  "Expense this Period" "Loss Total" "Expense Total" 
		  "Initial Reserve" "Outstanding Reserve" 
		  "Total Paid" "Incurred" "Status")
	    (mapcar (lambda (l)
		      ;; set the provice to just the first two letters
		     (let ((p (eighth l)))
		       (when (and 
			      (not (eql :null p))
			      (equal (aref p 3) #\-))
			 (setf (eighth l)
			       (subseq p 0 2)))			   
		      l))
		     (find-bordereau/excel (contract.contract-number contract)
			    start-date
			    end-date type)))))
   (if risk-type 
       (list risk-type) 
			   
       (risk-types))))

(defun claims-bordereau-html (*current-component* start-date end-date contract risk-type results)
  (with-output-to-string (yaclml:*YACLML-STREAM*)
    (<:h1 "Claims Bordereau")
    (<:div  
     (<:table
    (<:tr (<:td) (<:td))
    (<:tr (<:td "Contract: ") (<:td (display-inline contract)))
    (<:tr (<:td) (<:td))
    (<:tr (<:td "Start date: ") (<:td (display-inline start-date)))
    (<:tr (<:td "End date: ") (<:td (display-inline end-date)))
    (<:tr (<:td) (<:td))
    (<:tr (<:td) (<:td))
    (<:tr (<:td "Risk type: ") (<:td (display-inline risk-type)))
	  (<:tr (<:td) (<:td))))
   (<:br :style "clear:both")
   (<:table 
     (dolist (r results)
       (<:tr
	(<:td 
       (let ((report (rest r)))
	 (when (not (endp (rest (rest report))))
	   (<:table 
	    :style "font-size:7pt;width:100%;margin:0px;padding:0px;"
	    (<:tr
	     (<:td :colspan 
		    (format nil 
			    "~A" 
			    (length (car (rest r))))
		   (<:b (<:as-html (risk-type.type-name (first r))))))
	   (render-report-viewer-claims (rest r)))))))))))

(defun render-report-viewer-claims (results)

   (<:tr
    (dolist (h (car results))
      (<:th :style "background-color:#FFFEAB;border:1em;" (<:as-html h))))

   (<:tr
    (dolist (h (car results))
      (declare (ignore h))
      (<:td)))
   
   (let ((totals (second results)))
  ;   (break "~A" (cdr (cdr results)))
     (loop for r in (cdr (cdr results))
	for n from 1
	do ;(<:tr :style (if (evenp n ) "background-color: #E1E6FF;" ""))
	  (<:tr 
	(dolist (v r)
	  
	  (<:td :style "padding:2px;" (with-active-descriptions (inline)
					(if (and  (numberp v)
						  (not (integerp v)))
					    (<:as-html (format nil "~$" v)) 
					    (display-inline v)))))))
     (when (cdr (cdr results))
       (<:tr 
	(dolist (total totals)
	  (<:td (<:as-html total))))
       (<:tr 
	(dolist (total totals)
	  (declare (ignore total))
	  (<:td " ")))
       )))

(defun %dl-contract-bordereau-excel (contract string)
  (let* ((name (cl-ppcre:regex-replace-all
		"/| " 
		(format nil "~A-~A-~A-~A" 
			(contract.contract-number contract)
			(multiple-value-bind (year month day) 
			    (simple-date:decode-date 
			    (contract.effective-date contract))
			  (format nil "~2,'0d-~2,'0d-~4,'0d"
				  day month year))
			(contract.agency-id contract)
			(local-time:format-timestring
			 nil 
			 (local-time:universal-to-timestamp (get-universal-time)) 
			 :format '(:year "-" :month "-" 
				   :day "T" :hour ":" 
				   :min ":" :sec)))
		"---"))
		      
	 (html-file (format nil "/tmp/~A-contract-bordereau.html" name))
	 (xls-file (format nil "/tmp/~A-contract-bordereau.xls" name)))
    
    (with-output-to-file (s  html-file
			    :if-exists :overwrite
			    :if-does-not-exist :create)
      (princ string s))
    
    (sb-ext:run-program "/usr/bin/ssconvert" `(,html-file
					       ,xls-file))

    
    (values name xls-file)))
    

(defaction download-contract-bordereau-excel (name xls-file)
  (download-file :content-type "application/vnd.ms-excel"
		   :name (format nil "~A~A-contract-bordereau.xls" name
				 (local-time:format-timestring
				  nil 
				  (local-time:universal-to-timestamp (get-universal-time)) 
				  :format '(:year "-" :month "-" 
					    :day "T" :hour ":" 
					    :min ":" :sec)))
		   :file xls-file))

(defaction view-bordereau-report/excel ((contract contract))
  (let ((value (call 'bordereau-date-range-component :contract contract)))
    (with-slots (start-date end-date contract risk-type) value
      (let* ((b (find-claims-bordereau start-date end-date contract risk-type))
	     (h (claims-bordereau-html $window start-date end-date contract risk-type b)))
	(multiple-value-bind (name xls-file) 
	    (%dl-contract-bordereau-excel contract h)
	  (call-component 
	   $window 
	   (make-instance 'report-window-download :name name :xls-name xls-file)))))))
