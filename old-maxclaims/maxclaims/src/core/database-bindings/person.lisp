(in-package :maxclaims)

(defclass province/state ()
  ((province-state-id :primary-key t)
   short-name
   long-name)
  (:table-name province-state)
  (:metaclass described-db-access-class))

(defclass person ()
  ((person-id :primary-key t)
   first-name 
   last-name
   company-name
   (contracts-as-agency :referenced-by contract 
			:on agency-id)
   (policies-as-insured :referenced-by policy 
			:on insured-id)
   address1 address2 city
   (province/state
    :references province/state
    :column province-state-id)
   province-state-id
   postal-zip-code
   home-phone work-phone fax-phone cell-phone email-address birth-date
   date-added date-modified postal-code)
  (:metaclass described-db-access-class))

(defmethod select-using-db-object ((new person))
  (let ((res (when (and (not (slot-boundp new 'first-name))
			(not (slot-boundp new 'last-name))			 
			(slot-boundp new 'company-name))
	       (let ((*string-select-where-search* 
		       (lambda (cn val) 
			 `(:and (:or (:is-null 'first-name)
				     (:= 'first-name ""))
				(:or (:is-null 'last-name)
				     (:= 'last-name ""))
				(:ilike ,cn ,(format nil "~A%" val))))))
		 (select-using-object new)))))
    (append res (call-next-method))))
