(defpackage :maxclaims/ecm-description/report/spreadsheet
  (:use))

(in-package :maxclaims/ecm-description)

(defclass spreadsheet-payee-week ()
  ((start-date 
    :initarg :start-date
    :initform (multiple-value-bind (year month) 
		  (simple-date:decode-timestamp 
		   (simple-date:universal-time-to-timestamp 
		    (get-universal-time)))
		(simple-date:encode-date year month 01)))
   (spreadsheet-type 
    :initarg :spreadsheet-type
    :initform nil
    :accessor spreadsheet-payee-week-spreadsheet-type)))

(defmethod insert-object ((o spreadsheet-payee-week))
  o)

(define-descriptions spreadsheet-payee-week
  (:default )
  (:heading (:value :label "Payee By Week"
		    :value 
		    :activate (h3)))
  (:view  
	 (start-date 
	  :label t
	  :active t
	  :type simple-date:date
	  :required t)
	 (spreadsheet-type
	  :label t
	  :type ssconvert-type
	  ))
  (:create  
	   (start-date 
	    :label t
	    :active t
	    :type simple-date:date
	    :required t)
	  
	   (spreadsheet-type
	    :label t
	    :type ssconvert-type
	    :select-objects (ssconvert-export-types)))
  (:create-heading (:value :label "create" :value "Payee Sanction by Week"
			   :activate (h3))))

(defclass spreadsheet-contract-bordereau (contract-bordereau)
  ((spreadsheet-type 
    :initarg :spreadsheet-type
    :initform nil
    :accessor spreadsheet-contract-bordereau-spreadsheet-type)))

(defclass spreadsheet-claim-bordereau (claim-bordereau)
  ((spreadsheet-type 
    :initarg :spreadsheet-type
    :initform nil
    :accessor spreadsheet-contract-bordereau-spreadsheet-type)))

(defclass spreadsheet-inter-hannover-bordereau (claim-bordereau)
  ((spreadsheet-type 
    :initarg :spreadsheet-type
    :initform nil
    :accessor spreadsheet-contract-bordereau-spreadsheet-type)))
   

(define-descriptions ssconvert-type
  (:default name value mime-type)
  (:inline 
   (max-ecm/gnumeric/ssconvert::name)))



(define-descriptions spreadsheet-contract-bordereau
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
	  :allow-null t)
	 (spreadsheet-type
	  :label t
	  :type ssconvert-type
	  ))
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
	    :select-default-to-first t
	    :allow-null t)
	   (spreadsheet-type
	    :label t
	    :type ssconvert-type
	    :select-objects (ssconvert-export-types)
	    :select-default-to-first t))
  (:create-heading (:value :label "create" :value "Casualty / Liability Bordereau Spreadsheet"
			   :activate (h3))))

(define-descriptions spreadsheet-claim-bordereau
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
	  :allow-null t)
	 (spreadsheet-type
	  :label t
	  :type ssconvert-type
	  ))
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
	   (spreadsheet-type
	    :label t
	    :type ssconvert-type
	    :select-objects (ssconvert-export-types)))
  (:create-heading (:value :label "create" :value "Property Bordereau Spreadsheet"
			   :activate (h3))))

(define-descriptions spreadsheet-inter-hannover-bordereau
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
	  :allow-null t)
	 (spreadsheet-type
	  :label t
	  :type ssconvert-type
	  ))
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
	   (spreadsheet-type
	    :label t
	    :type ssconvert-type
	    :select-objects (ssconvert-export-types)))
  (:create-heading (:value :label "create" :value "Inter-Hannover Bordereau Spreadsheet"
			   :activate (h3))))
