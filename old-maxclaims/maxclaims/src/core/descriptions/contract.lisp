(in-package :maxclaims)

(defun format-contract-bordereau-link (contract)
  (prog1 "" ;; must return a string.
    (<:ul
     (<:li (<ucw:a :action (view-bordereau-report contract) 
 	     (<:as-html "View Contract Bordereau")))
     (<:li (<ucw:a :action (view-bordereau-report/heading contract) 
	     (<:as-html "View Casaulty Contract Bordereau"))))))

(defun effective-date-contract-setter (v o)
  (prog1 
      (if (lol::unbound-slot-value-p v)
	  (slot-makunbound o 'effective-date)
	  (setf (slot-value o 'effective-date) v))
    (unless (slot-boundp o 'expiry-date)
      (setf (slot-value o 'expiry-date)
	    (simple-date:time-add 
	     (simple-date:encode-interval :year 1 :day -1)
	     v))
      )))

(define-description contract (description-for-contract)
  ((contract-number :validate (boundp))
   (effective-date :input (:type simple-date)
		  :value-formatter print-date
		  :active :when
		  :validate (boundp)
		  :setter effective-date-contract-setter)
   (expiry-date :input (:type simple-date)
		   :value-formatter print-date
		   :active :when
		   :validate (boundp))
   (agency :input (:type select-db-object :db-type person)
	   :validate (boundp)
	   :active :when)
   (syndicate :input (:type select-db-object :db-type person)
	   :active :when)
   (view-bordereau :value-formatter format-contract-bordereau-link
		   :function #'identity)
   (risks :attribute-class has-many-attribute
	  :active :when)
   (active-attributes :value '(contract-number 
			       (agency :activate (link-to-viewer))
			       (syndicate :activate (link-to-viewer))
			       view-bordereau
			       (effective-date)
			       (expiry-date)
			       (risks :attributes (type policy 
						   (claims :active t)))))))

(define-description contract (description-for-contract)
  ((active-attributes :value '(contract-number
			       (effective-date :active :when)
			       (expiry-date :active :when)
			       agency
			       syndicate)))
  (:in-description inline))

(define-description contract (description-for-contract)
  ((active-attributes :value '(contract-number 
			       effective-date
			       expiry-date
			       (agency 
				:attributes ((first-name :active :when) 
					     (last-name :active :when) 
					     (company-name :active :when)))
			       (syndicate 
				:attributes ((first-name :active :when) 
					     (last-name :active :when) 
					     (company-name :active :when))
				:active t))))
  (:in-description editable))

