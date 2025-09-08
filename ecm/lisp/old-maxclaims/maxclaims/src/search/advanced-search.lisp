(in-package #:maxclaims)

;;; Advanced Search Component

(defcomponent advanced-search-component () 
  ((search-object :initform nil)))

(defmethod render ((self advanced-search-component))
  (<:h2 "Advanced Search")
  (if (null (slot-value self 'search-object))
      (<ucw:form :action (ucw-core::refresh-component self)
		 (<ucw:select 
		  :writer (lambda (s) 
			    (setf (slot-value self 'search-object)
				  (make-instance s)))
		  (<ucw:option :value 'claim "Claim")
		  (<ucw:option :value 'policy "Policy")
		  (<ucw:option :value 'contract "Contract")
		  (<ucw:option :value 'person "Person/Company")
		  (<ucw:option :value 'claim-transaction "Transactions")
		  (<ucw:option :value 'timecard "Timecards")
		  (<ucw:option :value 'attachment "attachment"))
		 (<:submit))
      (with-active-descriptions (editable)

	(let ((object (slot-value self 'search-object)))
	  (<ucw:form :action (view-object (get-search-result (select-using-object object)))
 
		     (<:submit)
		     (<ucw:submit :action (answer nil ) "cancel")
		     (read-object object)
		     (<:submit))))))



(defmethod rofl::select-using-object-where-clause :around
    ((search-value string) slotd column-name)
  (if (not *string-select-where-search*)
      `(:or ,(call-next-method)
	    (:% ,column-name ,search-value))
      (if (eql t *string-select-where-search*)
	  (call-next-method)
	  (funcall *string-select-where-search* 
		   column-name
		   search-value))))