(defpackage :ecm/report/bordereau
  (:use :cl)
  (:use :maxclaims-ecm)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :max-ecm/gnumeric/spreadsheet
		#:make-spreadsheet-title
		#:create-spreadsheet
		#:create-spreadsheet-from-alist)
  (:import-from :ecm/entity/contract
		#:find-contract)
  (:export #:casualty-liability-bordereau
	   #:casualty-liability-bordereau-spreadsheet
	   #:property-bordereau
	   #:property-bordereau-spreadsheet))
(in-package :ecm/report/bordereau)

(defun remove-totals (bordereau)
  (remove 
    :null 
    bordereau
    :key (lambda (row) 
	   (cdr (assoc "Policy" row 
		       :test 'string-equal)))))

(defun set-???-to (bordereau &optional (to ""))
  (mapcar (lambda (row)
	    (mapcar (lambda (cons)
		      (if (equalp (cdr cons) "???")
			  (prog1 cons (setf (cdr cons) to))
			  cons))
		    row))
	  bordereau))

(defun bordereau (contract-id start-date end-date risk-type
		              &key
                    (fields (maxclaims/report/bordereau:default-bordereau-fields))
                    (syndicate-id nil))
  (let ((risk-type (if (or (eq :null risk-type)
			   (equalp "" risk-type))
		       nil
		       risk-type)))
    (assert (not (and contract-id syndicate-id))
            nil "Cannot have both syndicate and contract for a bordereau")
    (maxclaims/report/bordereau:find-bordereau
     contract-id
     :fields  fields
     :where `(:and ,(if contract-id
                        `(:= contract.contract-id ,contract-id)
                        `(:= contract.syndicate-id ,syndicate-id))
		   ,@(when risk-type
		       `((:= risk.risk-type-name ,risk-type))))
     :start-date start-date
     :end-date (postmodern:query
		(s-sql:sql-compile `(:SELECT (:- (:type ,end-date date) (:raw  "INTERVAL '1 second'"))))
		:single))))

(defun filename (prefix contract start-date end-date)
  (cl-ppcre:regex-replace-all
   "\\s"
   (cl-ppcre:regex-replace-all
    "/" 
    (format nil "~A-~A-from-~A-to-~A-on-~A" 
	          (if contract
                (getjso "contract_number" contract)
                "Syndicate")
	          prefix
	          start-date
	          end-date
	          (local-time:format-timestring
			       nil 
			       (local-time:universal-to-timestamp (get-universal-time)) 
			       :format '(:year "-" :month "-" 
				               :day "T" :hour ":" 
				               :min ":" :sec)))
    "---")
   "_"))

(defun spreadsheet
    (title contract-id start-date end-date risk-type
     &key
       (bordereau nil)
       (fn #'property-bordereau-filename)
       (name t)
       (syndicate-id nil)
       (start-row nil)
       (create-header t)
       (document (max-ecm/gnumeric/document:empty-document))
       (document-sheet-name nil)
       (use-document-columns nil)
       (format-date "~{~A-~A-~A~}"))
  "=> spreadsheet, filename"
  (let* ((bordereau (remove-totals bordereau))
	     (contract (when contract-id (find-contract contract-id)))
	     (effective (when contract-id (getjso "effective_date" contract)))
	     (expiry (when contract-id (getjso "expiry_date" contract)))
	     (agency (when contract-id (getjso "agency" contract)))
	     (number (when contract-id
                   (if (stringp contract-id)
                       (postmodern:query (:select (:string-agg 'contract-number "__")
                                                  :from 'contract
                                                  :where (:= 'contract-id
                                                             (:raw (format nil "ANY ('{~A}')" contract-id))))
                                         :single)
                     (getjso "contract_number" contract))))
	     (title (when title
		          (make-spreadsheet-title
		           :title `(,title . ,number)
		           :block `(("Contract"
			                 .
			                 (("Effective-date"
				               . ,effective)
			                  ("Expiry-date"
				               . ,expiry)))
			                ("Agency"
			                 . ,(if agency
				                    (ecm/ui/utility:corpus-name agency)
				                  ""))
			                ("Start Date" . ,start-date)
			                ("End Date" . ,end-date)
			                ,@(when risk-type
				                `(("Risk Type" . ,risk-type)))))))
         (syndicate (when syndicate-id
                      (pomo:query (:select (:person-name syndicate-id)) :single))))
    (values (create-spreadsheet 
	         bordereau
	         :name (when (eq name t)
		             (or number syndicate))
	         :title title
           :create-header create-header
	         :start-row start-row
	         :document document
	         :document-sheet-name document-sheet-name
	         :use-document-columns use-document-columns
             :format-date format-date)
	        (funcall fn (or syndicate contract) start-date end-date))))

(defun zurich-bordereau (contract-id syndicate-id start-date end-date risk-type)
  (mapcar (lambda (row) (nbutlast row 2))
          (bordereau contract-id start-date end-date risk-type
             :syndicate-id syndicate-id
	         :fields (maxclaims/report/bordereau::zurich-bordereau-fields))))

(defun zurich-bordereau-filename (contract start-date end-date)
  (filename (if (stringp contract)
                (format nil "~A Zurich" contract)
                "Zurich")
            (unless (stringp contract)
              contract)
            start-date end-date))

(defun zurich-bordereau-spreadsheet
    (contract-id syndicate-id start-date end-date risk-type
     &key (bordereau (lloyds-v5-bordereau
		                  contract-id nil start-date end-date risk-type)))
  (spreadsheet NIL #+(or)"Lloyds v5 Bordereau for"
	       contract-id start-date end-date risk-type
	       :fn #'zurich-bordereau-filename
           :syndicate-id syndicate-id
	       :bordereau (set-???-to bordereau)
	       ;; :document (max-ecm/gnumeric/document:document
		   ;;     :pathname *lloyds-v5-gnumeric-xml-pathname*)
	       ;; :document-sheet-name "Other terrs"
	       ;; :use-document-columns t
	       ;; :start-row 3

           ))

(defun lloyds-v5-bordereau (contract-id syndicate-id start-date end-date risk-type)
  (bordereau contract-id start-date end-date risk-type
             :syndicate-id syndicate-id
	           :fields (maxclaims/report/bordereau:lloyds-v5-bordereau-fields)))

(defun lloyds-v5-bordereau-filename (contract start-date end-date)
  (filename (if (stringp contract)
                (format nil "~A lloyds-v5" contract)
                "lloyds-v5")
            (unless (stringp contract)
              contract)
            start-date end-date))

(defparameter *lloyds-v5-gnumeric-xml-pathname*
  (merge-pathnames
   "spreadsheet/Claim\ Template\ V5\ 20170522.xml"
   (asdf:system-source-directory 
    :ecm)))

(defun lloyds-v5-bordereau-spreadsheet
    (contract-id syndicate-id start-date end-date risk-type
     &key
       (bordereau (lloyds-v5-bordereau
		               contract-id nil start-date end-date risk-type)))
  (spreadsheet NIL #+(or)"Lloyds v5 Bordereau for"
	       contract-id start-date end-date risk-type
	       :fn #'lloyds-v5-bordereau-filename
           :syndicate-id syndicate-id
	       :bordereau (set-???-to bordereau)
	       :document (max-ecm/gnumeric/document:document
			   :pathname *lloyds-v5-gnumeric-xml-pathname*)
	       :document-sheet-name "Other terrs"
	       :use-document-columns t
	       :start-row 3))

(defun enstar-bordereau (&rest args)
  (apply #'lloyds-v5-bordereau args))

(defun enstar-bordereau-spreadsheet
    (contract-id syndicate-id start-date end-date risk-type
     &key (bordereau (enstar-bordereau
		                  contract-id nil start-date end-date risk-type)))
  (spreadsheet NIL
	           contract-id start-date end-date risk-type
	           :fn #'lloyds-v5-bordereau-filename
             :syndicate-id syndicate-id
	           :bordereau (set-???-to bordereau)
	           :document (max-ecm/gnumeric/document:document
			              :pathname *lloyds-v5-gnumeric-xml-pathname*)
	           :document-sheet-name "Other terrs"
	           :use-document-columns t
	           :start-row 3
             :format-date "~{~A/~A/~A~}"))

(defun ascot-bordereau (contract-id syndicate-id start-date end-date risk-type)
  (bordereau contract-id start-date end-date risk-type
             :syndicate-id syndicate-id
	           :fields (maxclaims/report/bordereau:ascot-lloyds-v5-bordereau-fields)))

(defun ascot-bordereau-spreadsheet
    (contract-id syndicate-id start-date end-date risk-type
     &key (bordereau
           (ascot-bordereau
		        contract-id syndicate-id start-date end-date risk-type)))
  (spreadsheet NIL
	           contract-id start-date end-date risk-type
	           :fn #'lloyds-v5-bordereau-filename
             :syndicate-id syndicate-id
	           :bordereau (set-???-to bordereau)
             :format-date "~{~A/~A/~A~}"))

(defparameter *hiscox-gnumeric-xml-pathname*
  (merge-pathnames
   "spreadsheet/Hiscox Claim Data Standards Template.xml"
   (asdf:system-source-directory
    :ecm)))

(defun hiscox-bordereau-filename (contract start-date end-date)
  (filename (if (stringp contract)
                (format nil "~A hiscox" contract)
                "hiscox")
            (unless (stringp contract)
              contract)
            start-date end-date))

(defun hiscox-bordereau (contract-id syndicate-id start-date end-date risk-type)
  (bordereau contract-id start-date end-date risk-type
             :syndicate-id syndicate-id
	           :fields (maxclaims/report/bordereau:hiscox-bordereau-fields)))

(defun hiscox-bordereau-spreadsheet
    (contract-id syndicate-id start-date end-date risk-type
     &key (bordereau
           (hiscox-bordereau
		        contract-id syndicate-id start-date end-date risk-type)))
  (spreadsheet NIL
	           contract-id start-date end-date risk-type
	           :fn #'hiscox-bordereau-filename
             :syndicate-id syndicate-id
	           :bordereau (set-???-to bordereau)
             :document (max-ecm/gnumeric/document:document
			              :pathname *hiscox-gnumeric-xml-pathname*)
	           :document-sheet-name "Data Capture template"
	           :use-document-columns t
	           :start-row 2
             ;:create-header nil
             :format-date "~{~A/~A/~A~}"))

(defun HUB-Dale-bordereau (contract-id start-date end-date risk-type)
  (bordereau contract-id start-date end-date risk-type
	     :fields (maxclaims/report/bordereau:HUB-Dale-bordereau-fields)))

(defun HUB-Dale-bordereau-filename (contract start-date end-date)
  (filename "HUB-Dale" contract start-date end-date))

(defun HUB-Dale-bordereau-spreadsheet
    (contract-id start-date end-date risk-type
     &key (bordereau (HUB-Dale-bordereau
		      contract-id start-date end-date risk-type)))
  (spreadsheet NIL #+(or)"Lloyds v5 Bordereau for"
	       contract-id start-date end-date risk-type
	       :fn #'HUB-Dale-bordereau-filename
	       :bordereau (set-???-to bordereau)))


(defun lloyds-claim-bordereau (contract-id start-date end-date risk-type)
  (bordereau
   (if (stringp contract-id)
               `(:raw ,(format nil "ANY ('{~A}')" contract-id))
               contract-id)
   start-date end-date risk-type
   :fields (maxclaims/report/bordereau:llyods-bordereau-fields)))

(defun lloyds-claim-bordereau-filename (contract start-date end-date)
  (filename "lloyds-claim" contract start-date end-date))

(defparameter *llyods-gnumeric-xml-pathname* 
  (merge-pathnames 
   "spreadsheet/Lloyds Claims Reporting Template V4.xml"
   (asdf:system-source-directory 
    :ecm)))


(defun lloyds-claim-bordereau-spreadsheet
    (contract-id start-date end-date risk-type
     &key (bordereau (lloyds-claim-bordereau
		      contract-id start-date end-date risk-type)))
  (spreadsheet nil
	       contract-id start-date end-date risk-type
	       :fn #'lloyds-claim-bordereau-filename
	       :bordereau bordereau
	       :document (max-ecm/gnumeric/document:document
			  :pathname *llyods-gnumeric-xml-pathname*)
	       :document-sheet-name "Lloyds Bordereau"
	       :use-document-columns t
	       :start-row 3))

(defun commonwell-lloyds-bordereau (contract-id start-date end-date risk-type)
  (bordereau
   (if (stringp contract-id)
               `(:raw ,(format nil "ANY ('{~A}')" contract-id))
               contract-id)
   start-date end-date risk-type
   :fields (maxclaims/report/bordereau:llyods-bordereau-fields)))

(defun commonwell-lloyds-bordereau-filename (contract start-date end-date)
  (filename "commonwell-lloyds" contract start-date end-date))

(defparameter *commonwell-llyods-gnumeric-xml-pathname* 
  (merge-pathnames 
   "spreadsheet/Lloyds Claims Reporting Template V4.xml"
   (asdf:system-source-directory 
    :ecm)))


(defun commonwell-lloyds-bordereau-spreadsheet
    (contract-id start-date end-date risk-type
     &key (bordereau (commonwell-lloyds-bordereau
		      contract-id start-date end-date risk-type)))
  (spreadsheet nil
	       contract-id start-date end-date risk-type
	       :fn #'lloyds-claim-bordereau-filename
	       :bordereau (set-???-to bordereau)
	       :document (max-ecm/gnumeric/document:document
			  :pathname *commonwell-llyods-gnumeric-xml-pathname*)
	       :document-sheet-name "Lloyds Bordereau"
	       :use-document-columns t
	       :start-row 3))

(defun arch-bordereau (contract-id start-date end-date risk-type)
  (bordereau contract-id
   start-date end-date risk-type
   :fields (maxclaims/report/bordereau:arch-bordereau-fields)))

(defun arch-bordereau-filename (contract start-date end-date)
  (filename "arch" contract start-date end-date))

(defparameter *arch-gnumeric-xml-pathname* 
  (merge-pathnames 
   "spreadsheet/Arch-Bordereau.xml"
   (asdf:system-source-directory 
    :ecm)))


(defun arch-bordereau-spreadsheet
    (contract-id start-date end-date risk-type
     &key (bordereau (arch-bordereau
		      contract-id start-date end-date risk-type)))
  (spreadsheet nil
	       contract-id start-date end-date risk-type
	       :fn #'arch-bordereau-filename
	       :bordereau bordereau
	       :document (max-ecm/gnumeric/document:document
			  :pathname *arch-gnumeric-xml-pathname*)
	       :document-sheet-name "Arch Bordereau"
	       :use-document-columns t
	       :start-row 0))

(defun inter-hannover-bordereau (contract-id start-date end-date risk-type)
  (bordereau contract-id start-date end-date risk-type
	     :fields (maxclaims/report/bordereau:inter-hannover-bordereau-fields)))

(defun inter-hannover-bordereau-filename (contract start-date end-date)
  (filename "Inter-Hannover" contract start-date end-date))

(defun inter-hannover-bordereau-spreadsheet
    (contract-id start-date end-date risk-type
     &key (bordereau (property-bordereau
		      contract-id start-date end-date risk-type)))
  (spreadsheet "Inter-Hannover Bordereau for"
	       contract-id start-date end-date risk-type
	       :fn #'inter-hannover-bordereau-filename
	       :bordereau bordereau))

(defun white-oak-bordereau (contract-id start-date end-date risk-type)
  (bordereau contract-id start-date end-date risk-type
	     :fields (maxclaims/report/bordereau:white-oak-bordereau-fields)))

(defun white-oak-bordereau-filename (contract start-date end-date)
  (filename "white-oak" contract start-date end-date))

(defparameter *white-oak-gnumeric-xml-pathname* 
  (merge-pathnames 
   "spreadsheet/NEW-CANADA-BDX-Format-11022014.xml"
   (asdf:system-source-directory 
    :ecm)))

(defun white-oak-bordereau-spreadsheet
    (contract-id start-date end-date risk-type
     &key (bordereau (white-oak-bordereau
		      contract-id start-date end-date risk-type)))
  (let* ((bordereau (remove-totals bordereau))
	 (contract (find-contract contract-id))
	 (name (getjso "contract_number" contract))
	 (sheet (create-spreadsheet-from-alist 
		 name bordereau  
		 :start-row 11
		 :create-header? nil))
	 (document (max-ecm/gnumeric/document:document 
		    :pathname *white-oak-gnumeric-xml-pathname*))
	 (sheets (stp:find-recursively-if 
		  (max-ecm/gnumeric/xml:of-name "Sheets") document))
	 (first-sheet (stp:find-recursively-if 
		       (max-ecm/gnumeric/xml:of-name "Sheet") sheets)))
    
    (max-ecm/gnumeric/xml:adopt-children first-sheet sheet)

    document))

(defun property-bordereau (contract-id start-date end-date risk-type)
    (bordereau contract-id start-date end-date risk-type
	       :fields (maxclaims/report/bordereau:claim-bordereau-fields)))

(defun property-bordereau-filename (contract start-date end-date)
    (filename "property" contract start-date end-date))

(defun property-bordereau-spreadsheet
    (contract-id start-date end-date risk-type
     &key (bordereau (property-bordereau
		      contract-id start-date end-date risk-type)))

  (spreadsheet "Property Bordereau for"
	       contract-id start-date end-date risk-type
	       :fn #'property-bordereau-filename
	       :bordereau bordereau))

(defun casualty-liability-bordereau (contract-id start-date end-date risk-type)
  (let ((risk-type (if (or (eq :null risk-type)
			   (equalp "" risk-type))
		       nil
		       risk-type)))
    (maxclaims/report/bordereau:find-bordereau
     contract-id
     :where `(:and (:= contract.contract-id ,contract-id)
		 ,@(when risk-type
		     `((:= risk.risk-type-name ,risk-type))))
     :start-date start-date
     :end-date (postmodern:query
		(s-sql:sql-compile `(:SELECT (:- (:type ,end-date date) (:raw  "INTERVAL '1 second'"))))
				   :single))))


(defun casualty-liability-bordereau-filename (contract start-date end-date)
  (cl-ppcre:regex-replace-all
   "\\s"
   (cl-ppcre:regex-replace-all
     "/" 
     (format nil "~A-casualty-liability-bordereau-from-~A-to-~A-on-~A" 
	     (getjso "contract_number" contract)
		       start-date
		       end-date
		       (local-time:format-timestring
			nil 
			(local-time:universal-to-timestamp (get-universal-time)) 
			:format '(:year "-" :month "-" 
				  :day "T" :hour ":" 
				  :min ":" :sec)))
     "---")
   "_"))
(defun casualty-liability-bordereau-spreadsheet
    (contract-id start-date end-date risk-type
     &key (bordereau (casualty-liability-bordereau
		      contract-id start-date end-date risk-type)))
   "=> spreadsheet, filename"
   (let* ((bordereau (remove-totals bordereau))
	  (contract (find-contract contract-id))
	 (effective (getjso "effective_date" contract))
	 (expiry (getjso "expiry_date" contract))
	 (agency (getjso "agency" contract))
	 (number (getjso "contract_number" contract))
	 (title (make-spreadsheet-title 
		 :title `("Bordereau for" . ,number)
		 :block `(("Contract"
			   . 
			   (("Effective-date" 
			     . ,effective)
			    ("Expiry-date" 
			     . ,expiry)))
			  ("Agency" 
			   . ,(ecm/ui/utility:corpus-name agency))
			  ("Start Date" . ,start-date)
			  ("End Date" . ,end-date)
			  ,@(when risk-type
			      `(("Risk Type" . ,risk-type)))))))
    (values (create-spreadsheet 
	     bordereau
	     :name number
	     :title title)
	    (casualty-liability-bordereau-filename contract start-date end-date))))

