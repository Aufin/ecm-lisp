(defpackage :maxclaims/ecm-description/report/spreadsheet/arch-bordereau
  (:use))

(in-package :maxclaims/ecm-description)

(defclass spreadsheet-arch-bordereau (contract-bordereau)
  ((spreadsheet-type 
    :initarg :spreadsheet-type
    :initform nil
    :accessor spreadsheet-white-oak-bordereau-spreadsheet-type)))

(define-descriptions spreadsheet-arch-bordereau
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
      (contract :label "Contract"))
  (:heading (contract :label "Arch Reporting Bordereau for"
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
	  :type ssconvert-type))
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
  (:create-heading (:value :label "create" :value "Arch Reporting Bordereau Spreadsheet"
			   :activate (h3))))
