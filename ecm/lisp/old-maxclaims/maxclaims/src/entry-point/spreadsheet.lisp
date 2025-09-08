(defpackage :maxclaims/entry-point/spreadsheet
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

  (:import-from :maxclaims/web-display/report
		#:bordereau-report-page)
  (:import-from #:max-ecm/gnumeric/spreadsheet
		#:create-spreadsheet
		#:spreadsheet-title-title
		#:create-spreadsheet-from-alist
		#:create-spreadsheet-from-title
		#:make-spreadsheet-title)
  (:import-from :max-ecm/gnumeric/document
		#:document)
  (:import-from :max-ecm/gnumeric/ssconvert
		 #:find-ssconvert-type
		 #:ssconvert-type-pathname-type
		 #:ssconvert-type-value
		 #:ssconvert-type-mime-type)
  (:import-from :max-ecm/gnumeric/xml
		#:<>
		#:of-name
		#:adopt-children
		#:<gnm>)
    (:import-from :max-ecm/gnumeric/cell 
		#:<cell>
		#:cell-value-type
		#:cell-value-format
		#:cell-row
		#:cell-column)
  (:export #:spreadsheet-list))
  
(in-package :maxclaims/entry-point/spreadsheet)


(defun print-date (date)
  (ignore-errors  (multiple-value-bind (year month day) 
		      (simple-date:decode-date 
		       date)
		    (format nil "~2,'0d-~2,'0d-~4,'0d"
			    day month year))))
(defun spreadsheet-list ()
  (<:div 
   :class "container" 
   (<:h1 "Spreadsheet Reports")
   (<:ul
    (<:li 
     (<:a :href "create?create[type]=spreadsheet-llyods-bordereau&access[read-only]=false"
	  :target "_blank"
	  "Lloyds Claims Reporting Bordereau"))
    (<:li 
     (<:a :href "create?create[type]=spreadsheet-arch-bordereau&access[read-only]=false"
	  :target "_blank"
	  "Arch Reporting Bordereau"))
    (<:li 
     (<:a :href "create?create[type]=spreadsheet-contract-bordereau&access[read-only]=false"
	  :target "_blank"
	  "Casaulty/Liability Claims Bordereau"))
    (<:li 
     (<:a :href "create?create[type]=spreadsheet-claim-bordereau&access[read-only]=false"
	  :target "_blank"
	  "Property Claims Bordereau"))

        (<:li 
     (<:a :href "create?create[type]=spreadsheet-inter-hannover-bordereau&access[read-only]=false"
	  :target "_blank"
	  "Inter-Hannover Bordereau"))
    (<:li 
     (<:a :href "create?create[type]=spreadsheet-white-oak-bordereau&access[read-only]=false"
	  :target "_blank"
	  "White Oak Bordereau"))
    (<:li 
     (<:a :href "create?create[type]=spreadsheet-payee-week&access[read-only]=false"
	  :target "_blank"
	  "Payee Sanctions Report")))))

(defvar *spreadsheet-parameter-alist*)

(defun geta (value &optional (alist *spreadsheet-parameter-alist*))
  (cadr (assoc value alist :test #'string=)))

(defun risk-type-parameter ()
  (let ((r (geta "risk-type")))
    (unless (or (string-equal r "NULL")
		(string-equal r ""))
      r)))

(defun contract-bordereau (contract-id &rest args)
  (mapcar 
   #'rest 
   (remove 
    :null 
    (apply #'maxclaims/report/bordereau:find-bordereau
	   contract-id
	   :start-date (geta "start-date")
	   :end-date (geta "end-date")
	   :risk-type (risk-type-parameter)
	   args)
    	  :key (lambda (row) 
			 (cdr (assoc "Policy" row 
				     :test 'string-equal))))))

(defun contract-bordereau-spreadsheet
    (contract-id
     &key 
       (bordereau (contract-bordereau contract-id))
       (contract (maxclaims::find-object 'maxclaims::contract contract-id))
       (name (maxclaims::contract.contract-number contract))
       (title (make-spreadsheet-title 
	       :title `("Bordereau for" 
			. ,(maxclaims::contract.contract-number contract))
	       :block `(("Contract" . 
				    (("Effective-date" 
				      . ,(print-date (maxclaims::contract.effective-date 
						      contract)))
				     ("Expiry-date" 
				      . ,(print-date (maxclaims::contract.expiry-date 
						      contract)))
				     ("Agency" 
				      . ,(maxclaims::person-name
					  (maxclaims::find-object 
					   'maxclaims::person 
					   (maxclaims::contract.agency-id contract))))))
			("Start Date" ., (geta "start-date"))
			("End Date" ., (geta "end-date"))
			,@(let ((rt (let ((r (geta "risk-type")))
				      (unless (or (string-equal r "NULL")
						  (string-equal r ""))
					r))))
			       (when rt `(("Risk Type" . ,rt))))))))
  (create-spreadsheet 
   bordereau
   :name name
   :title title))

(defun contract-bordereau-filename (contract-id)
  (let ((contract (maxclaims::find-object 'maxclaims::contract contract-id)))
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

(defun white-oak-bordereau (contract-id &rest args)
  (remove 
    :null 
    (apply #'maxclaims/report/bordereau:find-bordereau
	   contract-id 
	   :fields (maxclaims/report/bordereau:white-oak-bordereau-fields)
	   :start-date (geta "start-date")
	   :end-date (geta "end-date")
	   :risk-type (risk-type-parameter)
	   args)
    	  :key (lambda (row) 
			 (cdr (assoc "Policy" row 
				     :test 'string-equal)))))

(defparameter *white-oak-gnumeric-xml-pathname* 
  (merge-pathnames 
   "spreadsheet/NEW-CANADA-BDX-Format-11022014.xml"
   (asdf:system-source-directory 
    :maxclaims-ecm)))


(defun white-oak-bordereau-spreadsheet
    (contract-id
     &key 
       (bordereau (white-oak-bordereau contract-id))
       (contract (maxclaims::find-object 'maxclaims::contract contract-id))
       (name (maxclaims::contract.contract-number contract))
       (title (make-spreadsheet-title 
	       :title `("Bordereau for" 
			. ,(maxclaims::contract.contract-number contract))
	       :block `(("Contract" . 
				    (("Effective-date" 
				      . ,(print-date (maxclaims::contract.effective-date 
						      contract)))
				     ("Expiry-date" 
				      . ,(print-date (maxclaims::contract.expiry-date 
						      contract)))
				     ("Agency" 
				      . ,(maxclaims::person-name
					  (maxclaims::find-object 
					   'maxclaims::person 
					   (maxclaims::contract.agency-id contract))))))
			("Start Date" ., (geta "start-date"))
			("End Date" ., (geta "end-date"))
			,@(let ((rt (let ((r (geta "risk-type")))
				      (unless (or (string-equal r "NULL")
						  (string-equal r ""))
					r))))
			       (when rt `(("Risk Type" . ,rt))))))))
  (let* ((sheet (create-spreadsheet-from-alist 
		 name bordereau  
		 :start-row 11
		 :create-header? nil))
	 (document (document 
		    :pathname *white-oak-gnumeric-xml-pathname*))
	 (sheets (stp:find-recursively-if 
		  (max-ecm/gnumeric/xml:of-name "Sheets") document))
	 (sheet-name-index 
	  (stp:find-recursively-if (max-ecm/gnumeric/xml:of-name "SheetNameIndex") document))
	 (first-sheet (stp:find-recursively-if 
		       (max-ecm/gnumeric/xml:of-name "Sheet") sheets)))
    
    (max-ecm/gnumeric/xml:adopt-children first-sheet sheet)

    document))

(defun white-oak-bordereau-filename (contract-id)
  (let ((contract (maxclaims::find-object 'maxclaims::contract contract-id)))
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

(defun llyods-bordereau (contract-id &rest args)
  (remove 
    :null 
    (apply #'maxclaims/report/bordereau:find-bordereau
	   (if (stringp contract-id)
               `(:raw ,(format nil "ANY ('{~A}')" contract-id))
               contract-id)
	   :fields (maxclaims/report/bordereau:llyods-bordereau-fields)
	   :start-date (geta "start-date")
	   :end-date (geta "end-date")
	   :risk-type (risk-type-parameter)
	   args)
    	  :key (lambda (row) 
			 (cdr (assoc "Policy" row 
				     :test 'string-equal)))))

(defparameter *llyods-gnumeric-xml-pathname* 
  (merge-pathnames 
   "spreadsheet/Lloyds Claims Reporting Template V4.xml"
   (asdf:system-source-directory 
    :maxclaims-ecm)))

(defun llyods-bordereau-spreadsheet
    (contract-id
     &key 
       (bordereau (llyods-bordereau contract-id))

       (name (if (stringp contract-id)                 
                 (postmodern:query (:select (:string-agg 'contract-number "__")
                                            :from 'contract
                                            :where (:= 'contract-id
                                                       (:raw (format nil "ANY ('{~A}')" contract-id))))
                                   :single)
                 (maxclaims::contract.contract-number
                  (maxclaims::find-object 'maxclaims::contract contract-id))))
       (contract (maxclaims::find-object 'maxclaims::contract contract-id))
       (title (make-spreadsheet-title 
	       :title `("Bordereau for" 
			. ,(maxclaims::contract.contract-number contract))
	       :block `(("Contract" . 
				    (("Effective-date" 
				      . ,(print-date (maxclaims::contract.effective-date 
						      contract)))
				     ("Expiry-date" 
				      . ,(print-date (maxclaims::contract.expiry-date 
						      contract)))
				     ("Agency" 
				      . ,(maxclaims::person-name
					  (maxclaims::find-object 
					   'maxclaims::person 
					   (maxclaims::contract.agency-id contract))))))
			("Start Date" ., (geta "start-date"))
			("End Date" ., (geta "end-date"))
			,@(let ((rt (let ((r (geta "risk-type")))
				      (unless (or (string-equal r "NULL")
						  (string-equal r ""))
					r))))
			       (when rt `(("Risk Type" . ,rt))))))))

  (create-spreadsheet 
   bordereau 
   :title nil
   :start-row 3
   :name name 
   :document (document 
            :pathname *llyods-gnumeric-xml-pathname*)
 :document-sheet-name "Lloyds Bordereau"
 :use-document-columns t
   ))


  #+(OR)(let* ((sheet (create-spreadsheet-from-alist 
		 name bordereau  
		 :start-row 0
		 :create-header? t))
	 (document (document 
		    :pathname *llyods-gnumeric-xml-pathname*))
	 (sheets (stp:find-recursively-if
		  (max-ecm/gnumeric/xml:of-name "Sheets") document))
#+(or)	 (sheet-name-index 
	  (stp:find-recursively-if (max-ecm/gnumeric/xml:of-name "SheetNameIndex") document))
	 (llyods-sheet (first (stp:filter-children 
                        (lambda (sheet)
                          (and (funcall (max-ecm/gnumeric/xml:of-name "Sheet") sheet)
                               (let ((name (stp:find-child-if (max-ecm/gnumeric/xml:of-name "Name")
                                                              sheet)))
                                 (string-equal "Llyods Bordereau" (stp:string-value name)))))
                        sheets))))
    
    (max-ecm/gnumeric/xml:adopt-children llyods-sheet sheet)

    document)
(defun llyods-bordereau-filename (contract-id)
  (flet ((fn (contract)
             (cl-ppcre:regex-replace-all
	       "/| " 
	       (format nil "~A-~A-~A" 
		       (maxclaims::contract.contract-number contract)
		       (ignore-errors  (multiple-value-bind (year month day) 
					   (simple-date:decode-date 
					    (maxclaims::contract.effective-date contract))
					 (format nil "~2,'0d-~2,'0d-~4,'0d"
						 day month year)))
		       (ignore-errors (maxclaims::contract.agency-id contract)))
	       "---")))
    (concatenate
     'string
     (if (stringp contract-id)
         (apply 'concatenate 'string
                (loop for id in (postmodern:query (:select 'contract-id
                                                           :from 'contract
                                                           :where (:= 'contract-id
                                                                      (:raw (format nil "ANY ('{~A}')" contract-id))))
                                                  :column)
                   :collect (concatenate
                             'string
                             (fn (maxclaims::find-object 'maxclaims::contract id))
                             "__")))
         (fn (maxclaims::find-object 'maxclaims::contract contract-id)))
     		       (local-time:format-timestring
			nil 
			(local-time:universal-to-timestamp (get-universal-time)) 
			:format '(:year "-" :month "-" 
				  :day "T" :hour ":" 
				  :min ":" :sec)))))

(defun arch-bordereau (contract-id &rest args)
  (remove 
    :null 
    (apply #'maxclaims/report/bordereau:find-bordereau
	   (if (stringp contract-id)
               `(:raw ,(format nil "ANY ('{~A}')" contract-id))
               contract-id)
	   :fields (maxclaims/report/bordereau:arch-bordereau-fields)
	   :start-date (geta "start-date")
	   :end-date (geta "end-date")
	   :risk-type (risk-type-parameter)
	   args)
    	  :key (lambda (row) 
			 (cdr (assoc "Policy" row 
				     :test 'string-equal)))))

(defparameter *arch-gnumeric-xml-pathname* 
  (merge-pathnames 
   "spreadsheet/Arch-Bordereau.xml"
   (asdf:system-source-directory 
    :maxclaims-ecm)))

(defun arch-bordereau-spreadsheet
    (contract-id
     &key 
       (bordereau (arch-bordereau contract-id))

       (name (if (stringp contract-id)                 
                 (postmodern:query (:select (:string-agg 'contract-number "__")
                                            :from 'contract
                                            :where (:= 'contract-id
                                                       (:raw (format nil "ANY ('{~A}')" contract-id))))
                                   :single)
                 (maxclaims::contract.contract-number
                  (maxclaims::find-object 'maxclaims::contract contract-id))))
       (contract (maxclaims::find-object 'maxclaims::contract contract-id))
       (title (make-spreadsheet-title 
	       :title `("Bordereau for" 
			. ,(maxclaims::contract.contract-number contract))
	       :block `(("Contract" . 
				    (("Effective-date" 
				      . ,(print-date (maxclaims::contract.effective-date 
						      contract)))
				     ("Expiry-date" 
				      . ,(print-date (maxclaims::contract.expiry-date 
						      contract)))
				     ("Agency" 
				      . ,(maxclaims::person-name
					  (maxclaims::find-object 
					   'maxclaims::person 
					   (maxclaims::contract.agency-id contract))))))
			("Start Date" ., (geta "start-date"))
			("End Date" ., (geta "end-date"))
			,@(let ((rt (let ((r (geta "risk-type")))
				      (unless (or (string-equal r "NULL")
						  (string-equal r ""))
					r))))
			       (when rt `(("Risk Type" . ,rt))))))))

  (create-spreadsheet 
   bordereau 
   :title nil
   :start-row 0
   :name name 
   :document (document 
            :pathname *arch-gnumeric-xml-pathname*)
 :document-sheet-name "Arch Bordereau"
 :use-document-columns t
 ))

(defun arch-bordereau-filename (contract-id)
  (flet ((fn (contract)
             (cl-ppcre:regex-replace-all
	       "/| " 
	       (format nil "~A-~A-~A" 
		       (maxclaims::contract.contract-number contract)
		       (ignore-errors  (multiple-value-bind (year month day) 
					   (simple-date:decode-date 
					    (maxclaims::contract.effective-date contract))
					 (format nil "~2,'0d-~2,'0d-~4,'0d"
						 day month year)))
		       (ignore-errors (maxclaims::contract.agency-id contract)))
	       "---")))
    (concatenate
     'string
     (if (stringp contract-id)
         (apply 'concatenate 'string
                (loop for id in (postmodern:query (:select 'contract-id
                                                           :from 'contract
                                                           :where (:= 'contract-id
                                                                      (:raw (format nil "ANY ('{~A}')" contract-id))))
                                                  :column)
                   :collect (concatenate
                             'string
                             (fn (maxclaims::find-object 'maxclaims::contract id))
                             "__")))
         (fn (maxclaims::find-object 'maxclaims::contract contract-id)))
     		       (local-time:format-timestring
			nil 
			(local-time:universal-to-timestamp (get-universal-time)) 
			:format '(:year "-" :month "-" 
				  :day "T" :hour ":" 
				  :min ":" :sec)))))

(defun payee-week (&key (from (geta "start-date")))
  (postmodern:query 
   (:select 
    (:as 'claim-id (:raw "\"Claim Number\""))
    (:as (:person-name 'payee-id)
	 (:raw "\"Payee\""))
    (:as (:concat "$" 'amount) (:raw "\"Amount\""))
    (:as 'transaction-date (:raw "\"Date\""))
    (:as 'birth_date (:raw "\"Birth Date\""))
    (:as 'address1 (:raw "\"Address\""))
    (:as 'long_name (:raw "\"Province\""))
    :from 'claim-transaction
    :right-join 'person 
    :on (:= 'payee-id 'person-id)
    :left-join 'province-state
    :on (:= 'person.province-state-id
	   'province-state.province-state-id)
    :where 
    (:and (:>= 'transaction-date from)
	  (:<= 'transaction-date 
	       (:+ (simple-date:encode-interval :week 1) 
		   (:type from timestamp)))))
   :str-alists))

(defun payee-week-spreadsheet  
    (&key 
       (report (payee-week))
       (title (make-spreadsheet-title 
	       :title `("Payee Sanction by Week"
			. ,(geta "start-date")))))
  (create-spreadsheet report :title title :name "Payee Sanction"))

(defun time-recorded (value)
  (declare (ignore value))
  (create-spreadsheet 
   (maxclaims/report/time-recorded:TIME-RECORDED-REPORT 
    :start-date (geta "start-date")
    :end-date (geta "end-date"))
   :title (make-spreadsheet-title 
	       :title `("Block of Time" . "")
	       :block
	       `(("Start Date"
		 . ,(geta "start-date"))
		("End Date"
		 . ,(geta "end-date"))))
   :name "Time Recorded"))

(defun open-claims (value)
  (declare (ignore value))
  (create-spreadsheet 
   (maxclaims/report/open-claim:OPEN-CLAIM-REPORT)
   :title (make-spreadsheet-title 
	       :title `("Open Claims" . ""))
   :name "Open Claims"))

(defun interim (value)
  (declare (ignore value))
  (let* ((report (maxclaims/report/interim:interim-report 
		  :minimum-minutes (geta "minutes")))
	 (total (find "All Claims" report :key 'cdar 
		      :test #'string-equal))
	 (date (local-time:format-timestring 
		nil 
		(local-time:universal-to-timestamp 
		 (get-universal-time))
		:format local-time:+ISO-8601-DATE-FORMAT+)))
    (create-spreadsheet 
     (loop for ((head . name) . rest) :in report
	  :if (string-equal name "claim")
	  :collect rest)
     :name "Interim Amounts Report"
     :title (make-spreadsheet-title 
	     :block `(("Total Number of Claims"
		       ., (cdr (assoc "Claim Number" total 
						:test 'equalp)))
		      ("Total Amount of Minutes"
		       ., (cdr (assoc "Total Minutes" total 
				      :test 'equalp))))
	     
	     :title `("Interim" . ,date)))))
  
(defparameter *spreadsheets* 
  `(("interim-report"
     ,'interim
     , (lambda (f)
	 (declare (ignore f))
	 (substitute 
	  #\_ #\Space 
	  (format nil "Interim-Hours-on-~A"
		  (maxclaims/text-display:display 
			      (simple-date:universal-time-to-timestamp 
			       (get-universal-time))
			      :date-time)))))

    ("claim-open-report"
     ,'open-claims
     , (lambda (f)
	 (declare (ignore f))
	 (substitute 
	  #\_ #\Space 
	  (format nil "Open-Claims-on-~A"
		  (maxclaims/text-display:display 
			      (simple-date:universal-time-to-timestamp 
			       (get-universal-time))
			      :date-time)))))

    ("time-recorded-report"
     ,'time-recorded
     , (lambda (f)
	 (declare (ignore f))
	 (substitute 
	  #\_ #\Space 
	  (format nil "Time-Recorded_~A"
		  (maxclaims/text-display:display 
			      (simple-date:universal-time-to-timestamp 
			       (get-universal-time))
			      :date-time)))))
    ("payee-week"
     , (lambda (f)
	 (declare (ignore f))
	 (payee-week-spreadsheet))
       , (constantly "Payee-Sanction"))
     ("contract-bordereau"
     ,#'contract-bordereau-spreadsheet
     ,#'contract-bordereau-filename
     ,#'parse-integer)
    ("white-oak-bordereau"
     ,'white-oak-bordereau-spreadsheet
     ,#'white-oak-bordereau-filename
     ,#'parse-integer)
    ("llyods-bordereau"
     ,'llyods-bordereau-spreadsheet
     ,'llyods-bordereau-filename
     ,#'parse-integer)
    ("arch-bordereau"
     ,'arch-bordereau-spreadsheet
     ,'arch-bordereau-filename
     ,#'parse-integer)
    ("claim-bordereau"
     ,#'(lambda (id) 
	  (contract-bordereau-spreadsheet 
	   id 
	   :bordereau (contract-bordereau 
		       id 
		       :fields (maxclaims/report/bordereau::claim-bordereau-fields))))
     ,#'contract-bordereau-filename
     ,#'parse-integer)

    ("inter-hannover-bordereau"
     ,#'(lambda (id) 
	  (contract-bordereau-spreadsheet 
	   id 
	   :bordereau (contract-bordereau 
		       id 
		       :fields (maxclaims/report/bordereau::inter-hannover-bordereau-fields))))
     ,#'contract-bordereau-filename
     ,#'parse-integer)))
       
(defun spreadsheet-handler ()
  (let* ((*spreadsheet-parameter-alist* (http-parameters-as-alist "spreadsheet")))
    (multiple-value-bind (spreadsheet filename)
	(loop :for (name ss-function filename-fn &optional parser) :in *spreadsheets*
	   :do (let* ((value (geta name))
		      (value (when value (funcall (or parser 'identity) value))))		 
		 (when value 
		   (return 
		     (values (funcall ss-function value)
			     (funcall filename-fn value))))))
      
      (let* ((export-type (geta "type"))
	     (ssconvert-type 	      (if (and export-type
		       (< 0 (length  export-type)))
		  (find-ssconvert-type :value export-type)
		  (error "Need to choose a spreadsheet type")))
	     (export-filename (spreadsheet-filename spreadsheet filename ssconvert-type))
	     (mime-type (ssconvert-type-mime-type ssconvert-type))
	     (name (if export-filename
		       (pathname-name export-filename)
		       (error "No export filename")))
	     (type (pathname-type export-filename)))
	(prog1 t
	  (setf (hunchentoot:content-type*)
		mime-type)
	  (setf (hunchentoot:header-out 
		 "Content-Disposition")
		(format nil "attachment; filename=\"~A.~A\""
			name
			type))
	  (let ((output-stream (hunchentoot:send-headers)))
	    (alexandria:with-input-from-file 
		(stream export-filename
			:element-type '(unsigned-byte 8))
	      (alexandria:copy-stream 
	       stream output-stream))))))))
       

(defun spreadsheet-filename (spreadsheet filename ssconvert-type)
  (let* ((spreadsheet-filename 
	  (format nil "~A.xml" filename))
	 (export-filename (format nil "/tmp/~A.~A" 
			      filename (ssconvert-type-pathname-type 
					ssconvert-type))))
    
    (alexandria:with-output-to-file  
	(s (merge-pathnames spreadsheet-filename #P"/tmp/")
	   :if-exists :supersede)
      (stp:serialize 
       spreadsheet
       (cxml:make-character-stream-sink s)))
    
    (uiop:run-program 
     (format nil "/usr/bin/ssconvert --export-type=~A '~A' '~A'" 
	     (ssconvert-type-value ssconvert-type)
	     (merge-pathnames spreadsheet-filename #P"/tmp/") 
	     (merge-pathnames export-filename #P"/tmp/")))
    
    (merge-pathnames export-filename #P"/tmp/")))

(define-easy-handler (reports :uri "/ecm/spreadsheet")
    ()
  (maxclaims::with-adb 
    (spreadsheet-handler))
  #+(or)
    (handler-bind 
        ((error (lambda (c) 
                  (if (not maxclaims::*debug-on-error*)
                      (return-from reports 
                        (with-user-html-page ()
                          (<:div 
                           :class "alert alert-error" 
                           (<:as-html (princ-to-string c))
                           (<:br)
                           (<:button :onclick "history.go(-1)"
                                     :class "alert"
                                     (<:as-html "< go back")))))))))
	(maxclaims::with-adb 
	  (spreadsheet-handler))
      ))

  
  
