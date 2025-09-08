(defpackage :ecm/entity/policy
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/database)
  (:export #:search-for-policy
	   #:find-policy))
(in-package :ecm/entity/policy)

(defun find-policy (policy-id)
  (ecm/database:query/return-jso
    (:SELECT (:jsi.policy-summary policy-id))))

(defun search-for-policy (term &key (limit :null))
  (let* ((start
	  (postmodern:query
	   (:select
	    'p
	    :from (:as 
		   (:limit (:select (:as (:jsi.policy-summary 'policy) 'p)
				    :from 'policy
				    :where (:or (:ilike 'policy-number (format nil "~A%" term))
						(:ilike (:person-name 'insured-id) (format nil "~A%" term))))
			   limit)
		   'fps))
	   :column))
	 (end (when (> (if (eq :null limit) 25 limit)
		       (length start))
		(postmodern:query
		 (:select
		  'p
		  :from (:as 
			 (:limit (:select (:as (:jsi.policy-summary 'policy) 'p)
					  :from 'policy
					  :where (:or (:ilike 'policy-number (format nil "%~A%" term))
						      (:ilike (:person-name 'insured-id) (format nil "%~A%" term))))
				 limit)
			 'fps))
		 :column))))
    (mapcar 'ecm/json:read-json-from-string (nconc start end))))
