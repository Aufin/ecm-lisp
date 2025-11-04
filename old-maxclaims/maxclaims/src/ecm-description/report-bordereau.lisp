(defpackage :maxclaims/ecm-description/report-bordereau
  (:use))

(in-package :maxclaims/ecm-description)

(defclass bordereau-report ()
  ((report :initarg :report
	   :reader bordereau-report)
   (start-date 
    :initarg :start-date
    :initform (multiple-value-bind (year month) 
		  (simple-date:decode-timestamp 
		   (simple-date:universal-time-to-timestamp 
		    (get-universal-time)))
		(simple-date:encode-date year month 01)))
   (end-date 
    :initarg :end-date
    :initform (multiple-value-bind (year month day) 
		  (simple-date:decode-timestamp 
		   (simple-date:universal-time-to-timestamp 
		    (get-universal-time)))
		(simple-date:encode-date year month day)))
   (risk-type 
    :initarg :risk-type
    :accessor bordereau-report-risk-type
    :initform nil)
   (as-excel 
    :initarg :as-excel
    :initform NIL
    :accessor bordereau-report-as-excel)))

(defclass contract-bordereau (bordereau-report)
  ((report :initarg :report
	   :reader contract-bordereau)
   (contract :initarg contract
	     :initarg :contract)))

(defclass claim-bordereau (contract-bordereau)
  ())

(defclass inter-hannover-bordereau (contract-bordereau)
  ())

(defclass agency-bordereau (bordereau-report)
  ((report :initarg :report
	   :reader contract-bordereau)
   (agency :initarg :agency
	   :initarg :contract)))

(defclass contracts-bordereau (contract-bordereau)
  ((contracts-textarea :initarg contracts-textarea)
   (contracts :initarg :contracts 
	      :accessor contracts-bordereau-contracts)))

(defclass contracts-bordereau-contracts ()  
  ((contract-number :initarg :contract-number)
   (contracts :initarg :contracts
	      :reader contracts)))
   
(define-descriptions contracts-bordereau-contracts
  (:default )
  (:inline 
   (contract-number 
    :label t)
   (contracts
    :label "Contracts"
    :as-table t
    :edit nil)))

(define-descriptions contracts-bordereau
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
      (contract :label "Contract"))
  (:heading (:value :label "Bordereau"
		    :value "Contracts & Payee"
		    :activate (h3)))
  (:view-tab)
  (:view  
   (start-date 
    :label t
    :active t
    :type simple-date:date
    :required t)
   (end-date 
    :label t
    :active t
    :type simple-date:date
    :required t) 
   (risk-type 
    :label t
    :type risk-type 
    :select-objects (maxclaims::risk-types)
    :allow-null t))
  
  (:create (contracts-textarea
	    :label t
	    :active t
	    :textarea (:rows 10)
	    :required t) 
	   (start-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t)
	   (end-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t) 
	   (risk-type 
	    :label t
	    :type risk-type 
	    :select-objects (maxclaims::risk-types)
	    :allow-null t)
	   (contracts
	    :label "Selected Contracts"
	    :edit nil)
	    )
  (:create-heading (:value :label "create" :value "Contracts Bordereau with Payee Amount"
			   :activate (h3))))



(define-descriptions contract-bordereau
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
      (contract :label "Contract"))
  (:heading (contract :label "Bordereau for"
		      :attributes (contract-number)
		    :activate (h3)))
  (:view (contract
	  :label t
	  :active t
	  :type contract
	  :required t) 
	 (start-date 
	  :label t
	  :active t
	  :type simple-date:date
	  :required t)
	 (end-date 
	  :label t
	  :active t
	  :type simple-date:date
	  :required t) 
	 (risk-type 
	  :label t
	  :active :when
	  :allow-null t))
  (:create (contract
	    :label t
	    :active t
	    :select-objects (:search maxclaims::string-search-contract)
	    :type contract
	    :required t) 
	   (start-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t)
	   (end-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t) 
	   (risk-type 
	    :label t
	    :type risk-type 
	    :select-objects (maxclaims::risk-types)
	    :allow-null t)
	   (as-excel
	    :label "Download as Excel"
	    :type cl:boolean))
  (:create-heading (:value :label "create" :value "Casualty / Liability Bordereau"
			   :activate (h3))))
    

(defmethod object-attribute-value ((l contract-bordereau) 
				   (attribute-name (eql 'contract))
				   &key &allow-other-keys)
  (maxclaims::find-object 'maxclaims::contract (call-next-method)))

(defmethod insert-object ((o contract-bordereau))
  o)

(define-descriptions agency-bordereau
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
      (agency :label "Agency"))
  (:heading (agency :label "Bordereau for"
		    :activate (h3)))
  (:view (agency
	  :label t
	  :active t
	  :type person
	  :required t) 
	 (start-date 
	  :label t
	  :active t
	  :type simple-date:date
	  :required t)
	 (end-date 
	  :label t
	  :active t
	  :type simple-date:date
	  :required t) 
	 (risk-type 
	  :label t
	  :active :when
	  :allow-null t))
  (:create (agency
	    :label t
	    :active t
	    :select-objects (:search maxclaims::string-search-agency)
	    :type person
	    :required t) 
	   (start-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t)
	   (end-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t) 
	   (risk-type 
	    :label t
	    :type risk-type 
	    :select-objects (maxclaims::risk-types)
	    :allow-null t))
  (:create-heading (:value :label "create" :value "Agency Bordereau"
			   :activate (h3))))


(defmethod object-attribute-value ((l agency-bordereau) 
				   (attribute-name (eql 'agency))
				   &key &allow-other-keys)
  (maxclaims::find-object 'maxclaims::person (call-next-method)))

(defmethod insert-object ((o agency-bordereau))
  o)	

(define-descriptions claim-bordereau
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
      (contract :label "Contract"))
  (:heading (contract :label "Bordereau for"
		      :attributes (contract-number)
		    :activate (h3)))
  (:view (contract
	  :label t
	  :active t
	  :type contract
	  :required t) 
	 (start-date 
	  :label t
	  :active t
	  :type simple-date:date
	  :required t)
	 (end-date 
	  :label t
	  :active t
	  :type simple-date:date
	  :required t) 
	 (risk-type 
	  :label t
	  :active :when
	  :allow-null t))
  (:create (contract
	    :label t
	    :active t
	    :select-objects (:search maxclaims::string-search-contract)
	    :type contract
	    :required t) 
	   (start-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t)
	   (end-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t) 
	   (risk-type 
	    :label t
	    :type risk-type 
	    :select-objects (maxclaims::risk-types)
	    :allow-null t)
	   (as-excel
	    :label "Download as Excel"
	    :type cl:boolean))
  (:create-heading (:value :label "create" :value "Claim Bordereau"
			   :activate (h3))))

(define-descriptions inter-hannover-bordereau
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
      (contract :label "Contract"))
  (:heading (contract :label "Bordereau for"
		      :attributes (contract-number)
		    :activate (h3)))
  (:view (contract
	  :label t
	  :active t
	  :type contract
	  :required t) 
	 (start-date 
	  :label t
	  :active t
	  :type simple-date:date
	  :required t)
	 (end-date 
	  :label t
	  :active t
	  :type simple-date:date
	  :required t) 
	 (risk-type 
	  :label t
	  :active :when
	  :allow-null t))
  (:create (contract
	    :label t
	    :active t
	    :select-objects (:search maxclaims::string-search-contract)
	    :type contract
	    :required t) 
	   (start-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t)
	   (end-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t) 
	   (risk-type 
	    :label t
	    :type risk-type 
	    :select-objects (maxclaims::risk-types)
	    :allow-null t)
	   (as-excel
	    :label "Download as Excel"
	    :type cl:boolean))
  (:create-heading (:value :label "create" :value "Inter-Hannover Bordereau"
			   :activate (h3))))
  
