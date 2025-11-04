(defpackage :ecm/entity/corpus
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/database)
  (:export #:corpus-name-as-string
	   #:corpus-email-address
	   #:find-corpus
	   #:create-corpus
	   #:search-for-corpus
	   #:list-provinces))
(in-package :ecm/entity/corpus)

(defun create-corpus (&key first-name last-name company-name
			  birth-date
			  address-line-1 address-line-2
			  city province-id postal-code
			email-address home-phone work-phone fax cell-phone)
  (ecm/database:query/return-jso
    (:insert-into
     'person 
     :set 'first-name (or first-name :null)
     'last-name (or last-name :null)
     'company-name (or company-name :null)
     'birth-date (or birth-date :null)
     'address1 (or address-line-1 :null)
     'address2 (or address-line-2 :null)
     'city (or city :null)
     'province-state-id (or province-id :null)
     'postal-code (or postal-code :null)
     'email-address (or email-address :null)
     'home-phone (or home-phone :null)
     'work-phone (or work-phone :null)
     'fax-phone (or fax :null)
     'cell-phone (or cell-phone :null)
     :returning (:jsi.corpus-summary 'person.*))))
(defgeneric corpus-name-as-string (corpus &key company-name
					    province)
  (:method ((corpus ecm/json:jso)  &key (company-name t)
				     (province t))
    (let* ((first-name (getjso "first_name" corpus))
	   (last-name (getjso "last_name" corpus))
	   (company-name (when company-name (getjso "company_name" corpus)))
	   (province (when province (getjso "province" corpus)))
	   (short-name (and province (getjso "short_name" province))))
      (with-output-to-string (s)
	(when first-name (princ first-name s))
	(when last-name
	  (princ #\Space s)
	  (princ last-name s))
	(when (and (or first-name last-name)
		   company-name)
	  (princ ", " s))
	(when company-name (princ company-name s))
	(when (and (or first-name last-name company-name)
		   short-name)
	  (princ ", " s))
	(when short-name (princ short-name s))))))

(defun corpus-email-address (corpus)
  (getjso "email_address" corpus))

(defun find-corpus (corpus-id)
  (ecm/database:query/return-jso
    (:select (:jsi.corpus-summary corpus-id))))

(defun search-for-corpus (term &key (limit :null))
  (let* ((start
	  (postmodern:query
	   (:select
	    'p
	    :from (:as 
		   (:limit
		     (:select (:as (:jsi.corpus-summary 'person) 'p)
			     :from 'person
			     :where (:or
				     (:ilike (:person-name 'person)
					     (format nil "~A%" term))))
			   limit)
		   'fps))
	   :column))
	 (end (unless start
		(postmodern:query
		 (:select
		  'p
		  :from (:as 
		   (:limit
		    (:order-by
		     (:select (:as (:jsi.corpus-summary 'person) 'p)
			     :from 'person
			     :where (:or
				     (:ilike (:person-name 'person)
					     (format nil "%~A%" term))
				     (:ilike 'last-name
					     (format nil "~A%" term))
				     (:ilike 'company-name
					     (format nil "~A%" term))
				     (:ilike 'first-name
					     (format nil "~A%" term))
				     ))
		     (:is-null 'first-name))
		    limit)
		   'fps))
	   :column))))
    (mapcar 'ecm/json:read-json-from-string (nconc start end))))

(defun list-provinces ()
  (ecm/database:query/return-jso
    (:select (:json-agg (:to-json 'province-state.*)) :from 'province_state)))
