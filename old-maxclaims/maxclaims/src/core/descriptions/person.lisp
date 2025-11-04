(in-package :maxclaims)

(define-layered-class adjuster-person (slot-definition-attribute))

(defun adjusterp (person)
  (when (persistentp person)
    (query 
     (:limit 
      (:select 'adjuster-id :from 'claim 
	       :where (:= 'adjuster-id 
			  (person.person-id person))) 
      1) :row)))

(defun adjuster-user-id (person)
  (when (adjusterp person)
    (car (query 
     (:limit 
      (:select 'app-user-id :from 'app-user
	       :where (:= 'person-id 
			  (person.person-id person))) 
      1) :row))))

(define-layered-method 
    lol::display-html-attribute-value 
  ((p person)
   (a adjuster-person))
  (if (or (app-user.admin $app-user)
	  (not (adjusterp p)))
      (contextl::call-next-layered-method)
      (<:td 
       (<:as-html (attribute-value a)))))

(define-description province/state (description-for-province/state)
  ((short-name :label nil)
   (long-name :label nil)
   (attribute-delimiter :value " - ")
   (active-attributes :value '(short-name long-name)))
  (:in-description inline))



(define-description person (description-for-person)
  ((lol::label :function (lambda (self)
			   (format nil "Person: ~A"
				   (person-name self))))
   (first-name 
    :attribute-class text-slot-attribute)
   (last-name 
    :attribute-class text-slot-attribute)
   (company-name 
    :attribute-class text-slot-attribute)
   (policies-as-insured 
    :attribute-class has-many-attribute
    :active :when
    :attributes ((policy-number)
		 (claims :active t
			 :attribute-class list-slot-attribute
			 :deactivate (inline)
			 :item (:activate (link-to-viewer)
				:attributes (claim-id date-of-loss)))))
   (province/state :input (:type select-db-object 
			   :db-type province/state :size 1)
		   :activate (prepopulated-select)
		   :deactivate (writeable-select))
   (lol::active-attributes
    :value '((first-name :label nil :active :when)
	     (last-name :label nil :active :when)
	     (company-name :label nil :active :when)
	     (address1 :active :when) 
	     (address2 :active :when) 
	     (city :active :when)
	     (province/state :active :when) 
	     (postal-zip-code :active :when)
	     (home-phone :active :when) 
	     (work-phone :active :when) 
	     (fax-phone :active :when) 
	     (cell-phone :active :when) 
	     (email-address :active :when) 
	     (birth-date :active :when)
	     (contracts-as-agency 
	      :active :when
	      :attributes ((list :item (:activate (link-to-viewer)))))
	     (policies-as-insured)))))

(define-description person (description-for-person)
  ((active-attributes :value '((first-name :label nil :active :when)
			       (last-name :label nil :active :when)
			       (company-name :label nil :active :when)
			       (city :label nil :active :when)
			       (province/state :label nil :active :when
				:attributes (short-name))))
   (attribute-delimiter :value " "))
  (:in-description inline))

(define-description person (description-for-person)
  ((f :attribute-class adjuster-person 
      :slot-name first-name 
      :label "First name")
   (l :attribute-class adjuster-person 
      :slot-name last-name
      :label "Last name")
   (c :attribute-class adjuster-person 
      :slot-name company-name
      :label "Company name")
   (active-attributes :value '(f l c
			       address1 address2 city province/state
			       postal-zip-code home-phone work-phone
			       fax-phone cell-phone email-address
			       birth-date))
   (attribute-delimiter :value " "))
  (:in-description editable))