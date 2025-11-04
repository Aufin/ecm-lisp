(in-package #:maxclaims)

(defclass record-constraint-violation ()
  ((table :initarg :table :accessor table)))

(defclass column-constraint-violation (record-constraint-violation)
  ((column :initarg :column :accessor column)))

(defgeneric malformed-record-count (violation))
(defgeneric find-malformed-records (violation))
(defgeneric malformed-record-anchor (violation))

(defcomponent malformed-record-editor (object-editor-component)
  ()
  (:metaclass described-component-class))

(defaction edit-malformed-object (object)
  (call 'malformed-record-editor :object object))

(defcomponent malformed-records-component ()
  ((violations :accessor violations
	       :initform `((claim (,@(mapcar (lambda (c) `(not-null-violation :column ,c))
					    '(date-of-loss
					      risk-id
					      status
					      cause
					      plaintiff-id))
				     (invalid-adjuster-violation :column adjuster-id)))
			   (claim-transaction ((not-null-violation :column amount)))
			   (contract ,(mapcar (lambda (c) `(not-null-violation :column ,c))
					      '(contract-number
						contract-year)))
			   (policy ,(mapcar (lambda (c) `(not-null-violation :column ,c))
					    '(insured-id
					      policy-number
					      effective-date
					      expiry-date
					      deductible)))
			   (timecard ((not-null-violation :column minutes))))))
  (:metaclass described-component-class))

(defcomponent malformed-records-search-results (search-results)
  ()
  (:metaclass described-component-class))

(defmethod render ((self malformed-records-component))
  (<:h1 "Constraint Violating Records")
  (dolist* ((type (&rest violations)) (violations self))
    (<:h2 (<:format "~:(~A~)" type))
    (<:ul (dolist* ((violation &rest args) violations)
	    (let ((violation (apply #'make-instance violation :table type args)))
	      (<:li (<ucw:a :action (edit-malformed-object
				     (get-search-result
				      (find-malformed-records violation)
				      :display-using 'malformed-records-search-results))
			    (<:strong :style "color: red"
				      (<:ah (malformed-record-count violation)))
			    (<:ah " ")
			    (<:ah (malformed-record-anchor violation)))))))))

;;; Search Results
(defmethod search-result-attributes ((self malformed-records-search-results) result)
  (with-active-descriptions (malformed-records-search-results)
    (with-described-object (result)
      (attribute-value (find-attribute (dynamic description)
				       'active-attributes)))))


;;; Editor
(defmethod editor-attributes ((self malformed-record-editor))
  (with-described-object ((object self))
    #+nil(break "~A" (attributes (dynamic description)))
    (attribute-value (find-attribute (dynamic description)
				     'active-attributes))))

(defmethod render :around ((self malformed-record-editor))
  (with-active-descriptions (malformed-record-editor)
    (call-next-method)))

;;; Search Results Descriptions
(define-description claim-transaction (description-for-claim-transaction)
  ((active-attributes 
    :value '(transaction-date
	     claim
	     transaction-type
	     transaction-heading
	     (cheque-number :active :when)
	     (amount :active :when))))
  (:in-description malformed-records-search-results))

(define-description timecard (description-for-timecard)
  ((active-attributes
    :value '((claim :active :when
	      :attributes (claim-id insured))
	     (date))))
  (:in-description malformed-records-search-results))

;;; Violation Classes

(defclass not-null-violation (column-constraint-violation)
  ())

(defmethod malformed-record-count ((nnv not-null-violation))
  (query (:select (:count '*)
		  :from (table nnv)
		  :where (:is-null (column nnv)))
	 :single))

(defmethod find-malformed-records ((nnv not-null-violation))
  (query-objects (table nnv)
		 (lambda (table fields)
		   `(:limit (:select ,@fields
				     :from ,table
				     :where (:is-null ',(column nnv)))
			    1000))))

(defmethod malformed-record-anchor ((nnv not-null-violation))
  (format nil " Records where ~:(~A~) is unset" (column nnv)))

;; todo: generalize (invalid-reference-violation)
(defclass invalid-adjuster-violation (column-constraint-violation)
  ((invalid-adjuster :initform (query (:select 'person_id
					       :from 'person
					       :where (:and (:= 'first-name "INVALID")
							    (:= 'last-name "ADJUSTER")))
				      :single!))))

(defmethod malformed-record-count ((v invalid-adjuster-violation))
  (query (:select (:count '*)
		  :from (table v)
		  :where (:= (column v) (slot-value v 'invalid-adjuster)))
	 :single))

(defmethod find-malformed-records ((v invalid-adjuster-violation))
  (query-objects (table v)
		 (lambda (table fields)
		   `(:limit (:select ,@fields
				     :from ,table
				     :where (:= ',(column v) ,(slot-value v 'invalid-adjuster)))
			    
			    1000))))

(defmethod malformed-record-anchor ((v invalid-adjuster-violation))
  (format nil "Records where Adjuster is invalid"))

;;; Malformed Record Editor Descriptions
(define-description claim (description-for-claim)
  ((edit-externals :attribute-class edit-claim-externals-attribute :value t)
   (active-attributes 
    :value '((claim-id :editp nil) 
	     edit-externals
	     (plaintiff :active t)
	     (adjuster :active t)
	     details
	     (date-of-loss :active t)
	     (cause :active t :editp t) ; there is no editor for this!
	     (status :active t) #|fixme: was status-code|#
	     (risk :editp t :active t
	      :attributes ((contract :attributes (contract-number))
			   (policy :attributes (policy-number insured)))))))
  (:in-description malformed-record-editor))

(define-description claim-transaction (description-for-claim-transaction)
  ((lol:active-attributes 
    :value '((claim :editp nil :active t :activate (link-to-viewer)
	      :attributes (claim-id) ; have to disable
				     ; insured... shows up as a pair
				     ; of <input>s for an unknown
				     ; reason
	      )
	     (transaction-date) 
	     (transaction-type)
	     (transaction-heading)
	     (expense-type)
	     amount
	     payee
	     reference-number
	     cheque-number
	     schemes-advance-number)))
  (:in-description malformed-record-editor))

(define-description policy (description-for-policy)
  ((active-attributes 
    :value '((policy-number :active t) 
	     (insured :deactivate (link-to-viewer)
	      :attributes (first-name last-name company-name)
	      :active t)
	     (effective-date :active t)
	     (expiry-date :active t)
	     (deductible :active t)
	     (agent :deactivate (link-to-viewer))       
	     (branch :deactivate (link-to-viewer))
	     (underwriter :deactivate (link-to-viewer))
	     (sub-agent :deactivate (link-to-viewer))
	     (company :deactivate (link-to-viewer))
	     #+nil(risks))))
  (:in-description malformed-record-editor))

(define-description timecard (description-for-timecard)
  ((active-attributes
    :value '((claim :active :when
	      :attributes (claim-id insured)
	      :deactivate (editable)
	      :activate (link-to-viewer))
	     (date)
	     (app-user :deactivate (editable) :attributes (username)) 
	     (minutes :active :when)
	     (notes))))
  (:in-description malformed-record-editor))