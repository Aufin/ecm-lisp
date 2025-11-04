(defpackage :maxclaims/ecm-description/report 
  (:use) 
  (:import-from :maxclaims/report/cheque-register
		#:find-cheque-register
		#:find-contract-cheque-register)
  (:export #:find-cheque-register
	  	#:find-contract-cheque-register
	   #:select-agency-cheque-register
	   #:select-contract-cheque-register))

(in-package :maxclaims/ecm-description)

(defclass cheque-register ()
  ((report :initarg report
	   :reader cheque-register-report)
   (start-date :initarg start-date
	       :initform 
	       (multiple-value-bind (year month) 
		   (simple-date:decode-timestamp 
		    (simple-date:universal-time-to-timestamp 
		     (get-universal-time)))
		 (simple-date:encode-timestamp year month 01)))
   (end-date :initarg end-date
	     :initform (multiple-value-bind (year month day) 
			   (simple-date:decode-timestamp 
		   (simple-date:universal-time-to-timestamp 
		    (get-universal-time)))
			 (simple-date:encode-timestamp year month day)))
   (risk-type :initarg risk-type
	      :initform nil)))

(defclass cheque-register-report-line ()
  ((line :initarg line)))

(defun make-report-line (line)
  (make-instance 'cheque-register-report-line 'line line))

(defclass agency-cheque-register (cheque-register) 
  ((agency :initarg agency)))

(defclass contract-cheque-register (cheque-register)
  ((contract :initarg :contract 
	      :accessor contract-cheque-register-contract)))

(defun make-agency-cheque-register (report-list
			      &key 
				agency
				start-date 
				end-date
				risk-type)
  (make-instance
   'agency-cheque-register 
   'agency agency
   'report (mapcar #'make-report-line report-list)
   'start-date start-date 
   'end-date end-date
   'risk-type risk-type))

(defun make-contract-cheque-register 
    (report-list
     &key        
       contract
       start-date 
       end-date
       risk-type)
  (make-instance
   'contract-cheque-register 
   :contract contract
   'report (mapcar #'make-report-line report-list)
   'start-date start-date 
   'end-date end-date
   'risk-type risk-type))

(defun maxclaims/ecm-description/report:select-agency-cheque-register 
    (agency  
     &key 
       (start-date "2012-01-01")
       (end-date "2012-12-31")
       risk-type)
  (make-agency-cheque-register 
   (maxclaims/ecm-description/report:find-cheque-register 
    agency 
    :start-date start-date 
    :end-date end-date
    :risk-type risk-type)
   :agency agency
   :start-date start-date 
   :end-date end-date
   :risk-type risk-type))

(defun maxclaims/ecm-description/report:select-contract-cheque-register 
    (contract-id
     &key 
       (start-date "2012-01-01")
       (end-date "2012-12-31")
       risk-type)
  (make-contract-cheque-register
   (maxclaims/ecm-description/report:find-contract-cheque-register 
    contract-id
    :start-date start-date 
    :end-date end-date
    :risk-type risk-type)
   :contract contract-id
   :start-date start-date 
   :end-date end-date
   :risk-type risk-type))

(define-descriptions cheque-register 
  (:default (report :as-table t 
		    :label "AS"
		    :layers :default)))

(define-descriptions agency-cheque-register
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
      (agency :label "Cheque Register for"
	      ))
  (:heading (agency :label "Cheque Register for"
		    :activate (h3)))
  (:view-report
   (agency :label "Cheque Register for"
	   :activate (h3))
   (start-date :label t)
   (end-date :label t)
   (risk-type :label t
	      :active :when)
   
   )
    (:view
   (start-date :label t)
   (end-date :label t)
   (risk-type :label t
	      :active :when)
   )
  (:view-tab 
   (tab :label "Cheque Register"
	:tab :report))
  (:report (report :as-table t 		    
	   :label NIL
	   :attributes (payee 
			claim-number
			policy
			insured
			date-of-loss
			amount
			contract
			cheque-number
			status
			cheque-date
			expense-type)
		   :layers :default))
  (:create (agency 
	    :label t
	    :active t
	    :select-objects (:search maxclaims::string-search-person)
	    :type person) 
	   (start-date 
	    :label t
	    :active t
	    :type simple-date:timestamp
	    :required t)
	   (end-date 
	    :label t
	    :active t
	    :type simple-date:timestamp
	    :required t) 
	   (risk-type 
	    :label t
	    :type risk-type 
	    :select-objects (maxclaims::risk-types)
	    :allow-null t))
  (:create-heading (:value :label "create" :value "Agency Cheque Register"
			   :activate (h3))))

(define-descriptions contract-cheque-register
  (:create-heading (:value :label "create" :value "Contract Cheque Register"
			   :activate (h3)))
  (:create (contract
	    :label t
	    :active t
	    :select-objects (:search maxclaims::string-search-contract)
	    :type contract) 
	   (start-date 
	    :label t
	    :active t
	    :type simple-date:timestamp
	    :required t)
	   (end-date 
	    :label t
	    :active t
	    :type simple-date:timestamp
	    :required t) 
	   (risk-type 
	    :label t
	    :type risk-type 
	    :select-objects (maxclaims::risk-types)
	    :allow-null t))
  (:default (report :as-table t 		    
		    :label t
		    :layers :default))
  (:heading (contract :label "Contract Cheque Register for"
		      :activate (h3 link-to-viewer)))
  (:view
   (start-date :label t)
   (end-date :label t)
   (risk-type :label t
	      :active :when)

   )
  (:view-report
   (contract :label "Contract Cheque Register for"
	   :activate (h3 link-to-viewer))
   (start-date :label t)
   (end-date :label t)
   (risk-type :label t
	      :active :when)

   )
  (:view-tab 
   (tab :label "Cheque Register"
	:tab :report)
   (tab :label "Contract"
	:tab contract))
  (contract (contract :label NIL
		      :as-table t))
  (:report (report :as-table t 		    
	   :label NIL
	   :attributes (payee 
			claim-number
			policy
			insured
			date-of-loss
			amount
			contract
			cheque-number
			status
			cheque-date
			expense-type)
		   :layers :default))
  )

(defmethod object-attribute-value ((l  agency-cheque-register) 
				   (attribute-name (eql 'agency))
				   &key &allow-other-keys)
  (maxclaims::find-object 'maxclaims::person (call-next-method)))

(defmethod object-attribute-value ((l  contract-cheque-register) 
				   (attribute-name (eql 'contract))
				   &key &allow-other-keys)
  (maxclaims::find-object 'maxclaims::contract (call-next-method)))




(defmethod insert-object ((o cheque-register))
  o)

(defmethod object-attribute-value ((l cheque-register-report-line) attribute-name
				   &key &allow-other-keys)
  (cdr (assoc (substitute #\Space #\- (string-capitalize (string attribute-name)))
	      (slot-value l 'line):test #'string-equal)))

(define-descriptions cheque-register-report-line 
  (:default 
      (payee :label t)
      (claim-number :label t)
    (policy :label t)
    (insured :label t) 
    (date-of-loss :label t
		  :type simple-date:timestamp)
    (amount :label t)
    (contract :label t)
    (cheque-number :label t)
    (status :label t)
    (cheque-date :label t)
    (expense-type :label t))
    (:inline
      (payee :label t)
    ))
    




	

  
