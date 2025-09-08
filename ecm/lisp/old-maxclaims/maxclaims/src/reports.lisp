(in-package :maxclaims)

(defclass reports-component ()
  ()
  (:metaclass standard-component-class))

(defmethod render ((self reports-component))
  (<:div (<:fieldset 
	  (<:legend (<:as-html "Reports"))
	  (<:div 
	   :class "notes"
	   (<:p (<:i (<:as-html 
		      (format nil "The most recent transaction is from : ~A"
			      (format-simple-date
			       (query (:select (:max 'transaction-date)
				       :from 'claim-transaction)
				      :single)
			       :format local-time:+rfc-1123-format+))))))
	  
	  
	  (<:h3 (<:as-html "Cheque Register:"))
	  (<:as-html "Enter the name of the agency:")
	  (let ((agency ""))
	    (<ucw:form 
	     :action (view-cheque-register-report
		      (get-search-result  
		       (search-records agency :using '(string-search-agency))
		       :term agency
		       :display-using 'cheque-register-search-results))
	     (<ucw:input :accessor agency)
	     (<:submit :value "Find Agent")))
	  
	  (<:br)
	  (<:h3 (<:as-html "Claims Bordereau:"))
	  
	  (<:as-html "Enter the contract number:")
	  (let ((contract ""))
	    (<ucw:form 
	     :action (with-udb 
		       (view-bordereau-report
			(get-search-result  
			 (search-records contract :using '(string-search-contract))
			 :term contract
			 :display-using 'report-search-results)))
	     (<ucw:input :accessor contract)
	     (<:submit :value "Find Contract")))
	  
	  (<:br)

	  (<:h3 (<:strong :style "color: red" (<:ah "NEW"))
	  	(<:as-html "Casaulty/Liability Claims Bordereau"))
	  
	  (<:as-html "Enter the contract number:")
	  (let ((contract ""))
	    (<ucw:form 
	     :action (view-bordereau-report/heading
	  	      (get-search-result  
	  	       (search-records contract :using '(string-search-contract))
	  	       :term contract
	  	       :display-using 'report/heading-search-results))
	     (<ucw:input :accessor contract)
	     (<:submit :value "Find Contract")))

	  (<:br)

	  (<:h3 (<:strong :style "color: red" (<:ah "TEST "))
		(<:as-html "Casaulty/Liability Claims Bordereau:"))
	  
	  (<:as-html "Enter the contract number:")
	  (let ((contract ""))
	    (<ucw:form 
	     :action (get-search-result  
		       (search-records contract :using '(string-search-contract))
		       :term contract
		       :display-using 'contract-bordereau-report/heading-search-results)
	     (<ucw:input :accessor contract)
	     (<:submit :value "Find Contract")))

	  ;; (<:h3 (<:strong :style "color: red" (<:ah "AASDASDTEST "))
	  ;; 	(<:as-html "CasaultySASDASD/Liability Claims Bordereau:"))
	  
	  ;; (<:as-html "Enter the contract number:")
	  ;; (let ((contract ""))
	  ;;   (<ucw:form 
	  ;;    :action (view-contract-bordereau-report-html
	  ;; 	      (get-search-result  
	  ;; 	       (search-records contract :using '(string-search-contract))
	  ;; 	       :term contract
	  ;; 	       :display-using 'report/heading-search-results))
	  ;;    (<ucw:input :accessor contract)
	  ;;    (<:submit :value "Find Contract")))
	  
	  ;; (<:br)
	  
	  (<:h3 (<:as-html "Agency Bordereau Export:"))
	  (<:as-html "Enter the Agency")
	  (let ((agency ""))
	    (<ucw:form 
	     :action (view-bordereau-report
		      (get-search-result  
		       (search-records agency :using '(string-search-agency))
		       :term agency
		       :display-using 'report-search-results))
	     (<ucw:input :accessor agency)
	     (<:submit :value "Find Agency")))

	  (<:h3 (<:as-html "Agency Transaction Export:"))
	  (<:as-html "Enter the Agency")
	  (let ((agency ""))
	    (<ucw:form 
	     :action (view-bordereau-report
		      (get-search-result  
		       (search-records agency :using '(string-search-agency))
		       :term agency
		       :display-using 'report-search-results))
	     (<ucw:input :accessor agency)
	     (<:submit :value "Find Agency")))

	  (when (app-user.admin $app-user)
	    (<:h3 (<:as-html "Claims Bordereau as Excel:"))
	  
	    (<:as-html "Enter the contract number:")
	    (let ((contract ""))
	      (<ucw:form 
	       :action (with-udb 
			 (view-bordereau-report/excel 
			  (get-search-result  
			   (search-records contract :using '(string-search-contract))
			   :term contract
			   :display-using 'report-excel-search-results)))
	       (<ucw:input :accessor contract)
	       (<:submit :value "Find Contract"))))
	  
	  (<:br))))


(defclass date-range-component
    ()
  ((start-date :initarg :start-date 
	       :accessor start-date
	       :input (:type simple-date)
	       :validate (boundp))
   (end-date :initarg :end-date 
	     :accessor end-date
	     :input (:type simple-date)
	     :validate (boundp)))
  (:metaclass described-component-class))


(defclass select-risk-type (attribute-editor)
  ())

(defmethod display-html-attribute-editor (attribute (editor select-risk-type))
  (let ((writer (make-attribute-value-writer attribute)))
    (<ucw:select 
     :reader (attribute-value attribute)
     :writer writer
     (<ucw:option :value nil (<:as-html "All Types"))
     (arnesi:dolist* (val (risk-types))
       (<ucw:option :value val (<:as-html (risk-type.type-name val)))))))

(defclass risk-type-mixin ()
  ((risk-type :initarg :risk-type
	      :initform nil
	      :accessor select-risk-type
	      :edit t
	      :input (:type (or null select-risk) :class select-risk-type)))
  (:metaclass described-component-class))
  


(defmethod render ((self date-range-component))
  (with-active-descriptions (lol:validate)
    (<ucw:form 
     :action* (make-action 
	       (lambda () 
		 (with-udb 
		   (arnesi:with-call/cc  
		     (when (lol:validp self)
		       (answer self))))))   
     :target "_blank"
     (<:submit)
     (read-object self)
     (<:submit))))

(defcomponent report-viewer (standard-window-component)
  ((results :initarg :results :accessor results)
   (report-component :initarg :report-component)))

(defmethod render-report-viewer (self results)
  (<:table :style "font-size:7pt;width:100%;margin:0px;padding:0px;"
	   (<:tr
	    (dolist (h (car results))
	      (<:th :style "background-color:#FFFEAB;border:1em;" (<:as-html h))))
	   (loop for r in (cdr results)
	      for n from 1
	      do (<:tr :style (if (evenp n ) "background-color: #E1E6FF;" ""))
	      (dolist (v r)	
		(<:td :style "padding:2px;" (with-active-descriptions (inline)
					      (if (and  (numberp v)
							(not (integerp v)))
						  (<:as-html (format nil "~$" v)) 
						  (display self v))))))))
  
(defmethod render-html-body ((self report-viewer))
  (<:div
   (with-slots (report-component) self
     (display self report-component)))
  (<:br :style "clear:both")
  (render-report-viewer self (results self)))

;; this structure is used by print-date in bordereau.lisp -F
(defaction display-bordereau (contract-number start-date end-date)
  (flet ((mkdate (date)
	   (destructuring-bind (day mon year) (mapcar #'parse-integer (split-sequence #\/ date))
	     (simple-date:encode-date year mon day))))
    (call 'bordereau-viewer :records (find-bordereau contract-number start-date end-date))))
