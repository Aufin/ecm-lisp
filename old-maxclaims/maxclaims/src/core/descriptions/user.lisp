(in-package :maxclaims)

(define-description app-user (description-for-app-user)
  ((contracts :attribute-class has-many-attribute)
   (authority :input (:type has-one))
   (agency :attribute-class has-many-attribute)
   (person :input (:type select-db-object
			 :db-type person)
	   :label "Person or Company"
	   :validate (boundp)
	   :active :when)
   (admin :input (:type bool))
   (can-edit :input (:type bool))
   (username :validate (boundp))
   (enabled :function (lambda (object)
			(if (user-enabled-p object)
			    "YES"
			    "DISABLED")))
   (active-attributes
    :value '(username 
	     password 
	     (admin :editp t)
	     (can-edit :editp t)
	     
	     enabled
	     (person :attributes (first-name last-name company-name))
	     (contracts :attributes (contract) 
			:editp nil 
			:active :when)
	     (agency :attributes (agency)
		     :active :when)
	     authority))))

(define-description app-user (description-for-app-user)
  ((enabled :function (lambda (object)
			(if (user-enabled-p object)
			    object
			    nil))
	    :value-formatter render-disable-user-link
	    :editp nil
	    :label "Enabled/disabled")
   (person :active t
	   :validate (boundp))
      (active-attributes
    :value '(username 
	     password 
	     (admin :editp t)
	     (can-edit :editp t)
	     
	     enabled
	     (person :attributes (first-name last-name company-name)
		     :validate (boundp))
	     (contracts :attributes (contract) 
			:editp t
			:active t)
	     (agency :attributes (agency)
		     :active t))))
  (:in-description editable))

(define-description app-user (description-for-app-user)
  ((active-attributes
    :value '(username 
	     password 
	     (person :attributes (first-name 
				  last-name 
				  company-name)
		     :validate (boundp))
	     (admin :editp t)
	     (can-edit :editp t)
	     )))
  (:in-description object-creator-component))

(define-description app-user-authority (description-for-app-user-authority)
  ((user)
   (claim-incurred-limit :value-formatter $-formatter
			 :input (:type currency)
			 :validate (boundp))
   (active-attributes :value '(claim-incurred-limit))))

(define-description app-user-contract (description-for-app-user-contract)
  ((user :validate (boundp))
   (contract :active :when
	     :input (:type select-db-object
			   :db-type contract)
	     :validate (boundp))
   (active-attributes :value '(contract))))

(defun render-disable-user-link (val)
  (if val (prog1 "" (<ucw:a :action (setf (slot-value val 'password) "")
			    "Click to disable"))
      "Enter a password to enable this user"))

(defun %all-agencies (editor)
  (query-objects 'person (lambda (table fields)
			   (declare (ignore table))
			   `(:select ,@fields :from 'agency))))


(define-description app-user-agency (description-for-app-user-agency)
  ((user :validate (boundp))
   ;; fixme: constrain to only valid agencies
   (agency :active :when
	   :input (:type select-db-object :db-type person :size 1)
	   :activate ((prepopulated-select
		       :select-function %all-agencies))
	   :deactivate (writeable-select)
	   :validate (boundp)
	   :attributes (company-name))
   (active-attributes :value '(agency))))
