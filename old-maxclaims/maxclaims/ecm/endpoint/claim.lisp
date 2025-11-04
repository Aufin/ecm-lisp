(defpackage :ecm/endpoint/claim
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/hunchentoot)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/ui/claim #:claim/get)
  (:import-from :ecm/ui/claim/balance)
  (:import-from :ecm/ui/claim/clone)
  (:import-from :ecm/ui/claim/edit)
  (:import-from :ecm/ui/tabs #:<claim-tabs-page>)
  (:import-from :ecm/entity/claim)
  (:import-from :ecm/request-context)
  (:import-from :ecm/entity/risk)
  (:import-from :ecm/ui/diary))

(in-package :ecm/endpoint/claim)


(ecm/endpoint:define-endpoint clone-claim "ecm/claim/(\\d+)/clone$")

(defun clone-claim/get (claim-id)
  (ecm/ui/claim/clone:clone-claim-page claim-id))

(defun clone-claim/post (claim-id)
  (let ((contract-id (ecm/hunchentoot:parameter-or-nil "contract-id"))
        (subscription (ecm/hunchentoot:parameter-or-nil "subscription")))
    (ecm/request-context:with-request-context ()
      (ecm/request-context:send-json-response
       (ecm/entity/claim:clone-claim
        claim-id
        :contract-id contract-id
        :subscription subscription)))))

 

(ecm/endpoint:define-endpoint claim-card-balance-page
    "ecm/claim/(\\d+)/card/balance/page$")

(defun claim-card-balance-page/get (claim-id
			       &aux (claim-id (parse-integer claim-id)))
  (ecm/user:with-user ()
    (let* ((claim (ecm/entity/claim:find-claim-crux claim-id)))
      (ecm/ml:<> (ecm/ui/page:page :title "Balance")
	(ecm/ui/claim/balance:<claim-balance-card> claim)))))

(ecm/endpoint:define-endpoint claim-card-balance
    "ecm/claim/(\\d+)/card/balance$")

(defun claim-card-balance/get (claim-id
			       &aux (claim-id (parse-integer claim-id)))
  (ecm/user:with-user ()
    (let* ((claim (ecm/entity/claim:find-claim-crux claim-id)))
      (with-output-to-string (sexpml:*sexpml-output*)
	(ecm/ui/claim/balance:<claim-balance-card> claim)))))


(ecm/endpoint:define-endpoint claim
    "ecm/claim/(\\d+)$")

(ecm/endpoint:define-endpoint edit-claim
    "ecm/claim/(\\d+)/edit$")

(defmacro clet* ((name id prefix) &body body)
  (labels ((make (sym)
	     (let* ((sym (intern (format nil "~A~A"
					 prefix sym)))
		    (name (string-downcase sym)))
	       `(,sym (ecm/hunchentoot:parameter-or-nil ,name))))
	   (sym (sym)
	     (first (make sym))))
    (let ((ids (intern (string-upcase id))))
      `(let* ((,ids
		(ecm/hunchentoot:parameter-or-nil
		 ,(string-downcase id)  :identity #'parse-integer))
	      ,(make 'first-name)
	      ,(make 'last-name)
	      ,(make 'company-name)
	      ,(make 'birth-date)
	      ,(make 'address-line-1)
	      ,(make 'address-line-2)
	      ,(make 'city)
	      ,(make 'province-id)
	      ,(make 'postal-code)
	      ,(make 'email-address)
	      ,(make 'home-phone)
	      ,(make 'work-phone)
	      ,(make 'fax)
	      ,(make 'cell-phone) 
	      (,name (if ,ids
			 (ecm/entity/corpus:find-corpus ,ids)
			 (when (or ,(sym 'first-name)
				   ,(sym 'last-name)
				   ,(sym 'company-name))
			   (let ((,name (ecm/entity/corpus:create-corpus
					 :first-name ,(sym 'first-name)
					 :last-name ,(sym 'last-name)
					 :company-name ,(sym 'company-name)
					 :birth-date ,(sym 'birth-date)
					 :address-line-1 ,(sym 'address-line-1)
					 :address-line-2 ,(sym 'address-line-2)
					 :city ,(sym 'city)
					 :province-id ,(sym 'province-id)
					 :postal-code ,(sym 'postal-code)
					 :email-address ,(sym 'email-address)
					 :home-phone ,(sym 'home-phone)
					 :work-phone ,(sym 'work-phone)
					 :fax ,(sym 'fax)
					 :cell-phone ,(sym 'cell-phone))))
			     (setf ,ids (getjso "_id" ,name))
			     ,name)))))
	 
	 ,@body))))

(defun edit-claim/get (claim-id
 		       &aux (claim-id (parse-integer claim-id)))
  (ecm/user:with-user ()
    (ecm/ui/claim/edit:<claim-edit-page> claim-id)))

(defun edit-claim/post (claim-id
			&aux (claim-id (parse-integer claim-id)))
  (ecm/user:with-user
      ()
    (let* ((status (ecm/hunchentoot:parameter "claim-status"))
	   (examiner-id (parse-integer (ecm/hunchentoot:parameter "examiner-id")))
	   (lineage (ecm/hunchentoot:parameter-or-nil "lineage"))
	   (label (ecm/hunchentoot:parameter-or-nil "label"))
	   (date-of-loss (ecm/hunchentoot:parameter "date-of-loss"))
	   (date-claim-made (ecm/hunchentoot:parameter-or-nil "date-claim-made"))
	   (received  (ecm/hunchentoot:parameter-or-nil "received-time"))
	   (ack  (ecm/hunchentoot:parameter-or-nil "acknowledged-time"))
	   (deductible (ecm/hunchentoot:parameter-or-nil "deductible"))
	   (cause (ecm/hunchentoot:parameter-or-nil "cause"))
	   (authority (ecm/hunchentoot:parameter-or-nil "authority"))
	   (contacted (ecm/hunchentoot:parameter-or-nil "insured-contacted-time"))
	   (first-site (ecm/hunchentoot:parameter-or-nil "first-site-visit-time"))
	   (peer-reviewed (ecm/hunchentoot:parameter-or-nil
			   "peer-reviewed"))
	   (recovery-date (ecm/hunchentoot:parameter-or-nil
			   "recovery-date"))
	   (industry-class (ecm/hunchentoot:parameter
			    "industry-code"))
	   (industry (or (ecm/hunchentoot:parameter-or-nil
			  (concatenate 'string "ind" industry-class))
			 (ecm/hunchentoot:parameter-or-nil "ind")))
	   (subscription (ecm/hunchentoot:parameter-or-nil "subscription"))
	   (denial (when (ecm/hunchentoot:parameter-or-nil "denial")
		     t))
	   (date-of-denial (ecm/hunchentoot:parameter-or-nil
                            "date-of-denial"))
           (reason-for-denial (ecm/hunchentoot:parameter-or-nil
                               "reason-for-denial"))
	   (refer-to-underwriters
	     (when (ecm/hunchentoot:parameter-or-nil "refer-to-underwriters")
	       t))
	   (complaint (when (ecm/hunchentoot:parameter-or-nil "complaint")
		     t))
	   (date-of-complaint (ecm/hunchentoot:parameter-or-nil
                            "date-of-complaint"))
           (reason-for-complaint (ecm/hunchentoot:parameter-or-nil
                               "reason-for-complaint"))
	   (open-for-recovery
	     (when (ecm/hunchentoot:parameter-or-nil "open-for-recovery")
	       t))
	   (external-adjuster
	     (clet* (ext external-adjuster-id external-adjuster-)
	       ext))
	   (claimant
	     (clet* (cl claimant-id claimant-)
	       cl))
	   (restoration-firm-emergency
	     (clet* (cc restoration-firm-emergency-id restoration-firm-emergency-)
	       cc))
           (restoration-firm-repair
	     (clet* (cc restoration-firm-repair-id restoration-firm-repair-)
	       cc))
           (coverage-counsel
	     (clet* (cc coverage-counsel-id coverage-counsel-)
	       cc))
	   (defense-counsel
	     (clet* (dc defense-counsel-id defense-counsel-)
	       dc))
	   (line-of-business
	     (ecm/hunchentoot:parameter-or-nil "line-of-business"))
	   (coverage
	     (ecm/hunchentoot:parameter-or-nil "coverage")))
      (handler-case
	  (progn (ecm/entity/claim:update-claim
		  claim-id
		  :status status
		  :label (or label :null)
		  :line-of-business (or line-of-business :null)
		  :coverage (or coverage :null)
		  :adjuster-id examiner-id
		  :date-of-loss date-of-loss
		  :date-claim-made (or date-claim-made :null)
                  :lineage (or lineage :null)
		  :denial denial
                  :date-of-denial (or date-of-denial :null)
                  :reason-for-denial (or reason-for-denial :null)
		  :complaint (if (not complaint) :null
				 (ecm/json:write-json-to-string
				  (ecm/json:jso "date" date-of-complaint
						"reason" reason-for-complaint)))
				      
		  :insured-contacted-time (or contacted :null)
		  :first-site-visit-time (or first-site :null)
		 
		  :coverage-counsel-id (if coverage-counsel
					   (getjso "_id" coverage-counsel)
					   :null)
                  :restoration-firm-emergency-id (if restoration-firm-emergency
					             (getjso "_id" restoration-firm-emergency)
					             :null)
                  :restoration-firm-repair-id (if restoration-firm-repair
					          (getjso "_id" restoration-firm-repair)
					          :null)
		  :defense-counsel-id (if defense-counsel
					  (getjso "_id" defense-counsel)
					  :null)
		  :refer-to-underwriters refer-to-underwriters
		  :open-for-recovery open-for-recovery
		  :claim-received-time (or received :null)
		  :claim-acknowledged-time (or ack :null)
		  :deductible deductible
		  :cause (or cause :null)
		  :authority (or authority 0)
		  :peer-reviewed-date (or peer-reviewed :null)
		  :recovery-subrogation-date (or recovery-date :null)
		  :external-adjuster-id (if external-adjuster
					    (getjso "_id" external-adjuster)
					    :null)
		  :plaintiff-id (if claimant
				    (getjso "_id" claimant)
				    :null)
		  :subscription_percent (or subscription :null))

		 (let ((ex-industry
		         (postmodern:query (:select 'industry :from 'claim-ibc-code
					    :where (:= 'claim-id claim-id))
					   :single))
		       (industry (when (and industry industry-class)
				   (parse-integer industry))))

		   (cond ((and (not industry) ex-industry)
			  (postmodern:query (:delete-from 'claim-ibc-code
					     :where (:= 'claim-id claim-id))))
			 ((and ex-industry
			       (not (equalp industry ex-industry)))
			  (postmodern:query (:update
					     'claim-ibc-code
					     :set 'industry industry
					     :where (:= 'claim-id claim-id))))
			 ((and industry (not ex-industry))
			  (postmodern:query (:insert-into
					     'claim-ibc-code
					     :set
					     'industry industry
					     'claim-id claim-id)))))
			
			
			      
				   
		 (hunchentoot:redirect (format nil "/ecm/claim/~A" claim-id)
				       :protocol ecm/hunchentoot:*protocol*))
	(error (c)
	  (ecm/ui/claim/edit:<claim-edit-page>
	   claim-id
	   :error c
	   :status status
	   :label label
	   :line-of-business line-of-business
	   :coverage coverage
	   :examiner (ecm/entity/corpus:find-corpus examiner-id)
	   :date-of-loss date-of-loss
	   :date-claim-made date-claim-made
	   :denial denial
	   :coverage-counsel coverage-counsel
	   :defense-counsel defense-counsel
	   :refer-to-underwriters refer-to-underwriters
	   :open-for-recovery open-for-recovery
	   :received received
	   :acknowledged ack
	   :deductible deductible
	   :cause (if cause
		      (ecm/entity/claim:find-cause cause))
	   :claim-authority authority
	   :peer-reviewed-date peer-reviewed
	   :recovery-date recovery-date
	   :industry-class industry-class
	   :external-adjuster external-adjuster
	   :claimant claimant
	   :subscription subscription
	   :industry (if industry (parse-integer industry))))))))

(ecm/endpoint:define-endpoint clone-risk
    "ecm/claim/(\\d+)/clone-risk$")

(defun clone-risk/get (claim-id
 		       &aux (claim-id (parse-integer claim-id)))
  (ecm/user:with-user ()
    (let* ((risk (getjso "risk" (ecm/entity/claim:find-claim claim-id)))
	   (new-risk (ecm/entity/risk:clone-risk (getjso "_id" risk))))      
      (ecm/entity/claim:update-claim
       claim-id
       :risk-id (getjso "_id" new-risk))
      (ecm/hunchentoot:redirect
       (concatenate
	'string "/ecm/claim/"
	(princ-to-string claim-id)
	#+(or)"?edit-risk=true")))))
				     
(ecm/endpoint:define-endpoint claim-tabs
    "ecm/claim/(\\d+)/tabs$")

(defun claim-tabs/get (claim-id
		       &aux (claim-id (parse-integer claim-id)))
  (ecm/user:with-user ()
    (let* ((claim-crux-json
	    (ecm/json:null->nil
	     (postmodern:query
	     (:select (:jsi.claim-crux claim-id))
	     :single)))
	   (claim
	    (progn (unless claim-crux-json
		     (error "No CLaim Crux for ~A" claim-id))	    
		   (ecm/json:read-json-from-string
		    claim-crux-json))))
      (<claim-tabs-page> claim))))



(ecm/endpoint:define-endpoint claim-diaries
    "ecm/claim/(\\d+)/diary/?$")

(defun claim-diaries/get (claim-id
			  &aux (claim-id (parse-integer claim-id)))
  (ecm/user:with-user ()
    (ecm/ui/diary:<claim-diary-page> claim-id)))

(ecm/endpoint:define-endpoint claim-diary-create
    "ecm/claim/(\\d+)/diary/create$")


