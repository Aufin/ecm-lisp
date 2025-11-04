(defpackage :maxclaims/report/claim-authority
  (:use :cl)
  (:import-from :rofl 
		#:select)
  (:import-from :maxclaims
		  #:with-adb
		  #:risk-type.type-name)
  (:export 
	   #:select-claims-over-authority))

(in-package :maxclaims/report/claim-authority)

(defparameter *claim-authority-headings*
  '(("Claim Number" c.claim-id)
    ("Examiner" (:raw "person_name(find_person(c.adjuster_id))"))
    ("Incurred" (:+ (claim-outstanding-reserve c.claim-id (:type (:now) :date))
		 (:-  (:+ (claim-cheque-loss c.claim-id (:type (:now) :date))
			  (claim-cheque-expense c.claim-id (:type (:now) :date)))
		      (claim-salvage c.claim-id (:type (:now) :date))
		      (claim-subrogation c.claim-id (:type (:now) :date))
		      (claim-recovered-deductible c.claim-id (:type (:now) :date)))))
    ("Authority" ca.authority)))

(defparameter *claim-authority-query*
  '(:from (:as claim-authority ca)
    :left-join (:as claim c)
    :on (:= c.claim-id ca.claim-id)
    :where (:and 
	    (:not (:= ca.authority 0)))))

(defun claim-authority-query ()
  `(:select '* 
	    :from (:as 
		   (:order-by 
		    (:select 
		     ,@(mapcar #'heading-as *claim-authority-headings*)
		     ,@*claim-authority-query*)
		    'c.claim-id)
		   ca)
	    :where (:> (:raw ,(symbol-name '|"Incurred"|))
		       (:raw ,(symbol-name '|"Authority"|)) )))



(defstruct (claim-authority-heading (:type list))
  name s-sql)

(defun heading-as (heading)
  `(:as ,(claim-authority-heading-s-sql heading)
	(:raw ,(concatenate 'string "\""
		      (claim-authority-heading-name heading)
		      "\""))))

(defun select-claims-over-authority ()
  (postmodern:query
   (s-sql:sql-compile (claim-authority-query))
   :str-alists))
  

