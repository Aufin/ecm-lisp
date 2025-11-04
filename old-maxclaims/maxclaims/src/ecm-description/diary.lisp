(defpackage :maxclaims/ecm-description/diary (:use))
(in-package :maxclaims/ecm-description)

(defun insert-claim-diary (claim note
			   &key (action-date 
				 (claim.open-date claim)))
  (let ((users (person-ecm-users 
		(claim.adjuster claim))))
    (if (> (length users) 1)
	(error "Adjuster has ~A users associated with it. 
It has to be only one." (length users))
	(postmodern:execute 
	 (:insert-into 'diary-entry
		       :set 
		       'claim-id (claim.claim-id claim)
		       'note note
		       'app-user-id (app-user.app-user-id
				     (first users))
		       'action-date action-date)))))

(defun list-diary-users ()
  (sort (maxclaims::with-adb (maxclaims::select-objects
                   'app-user
                   :where '(:and login can-edit
                            (:not (:= username "admin"))
                            (:or (:in app-user-id (:select app-user-id :from 'diary-entry))
                                        (:in person-id (:select person_id :from person :where (:ilike (:person-name person) "%Max%")))))))
        #'string< :key #'maxclaims::app-user.username))

(defmethod update-object :after ((de diary-entry))
  ;; If it is processed, check if the claim is open.
  ;;  1) If it is open, check for an open diary
  ;;  2) If there is not an open diary, create one.
  (when (diary-entry.processed de)
    (let* ((claim (diary-entry.claim de))
	   (claim-status (claim-status.claim-status-type 
			  (claim.status claim))))
      (when (string-equal claim-status "Open")
	(let ((open-diary 
	       (postmodern:query 
		(:limit 
		 (:select 
		  '* :from 'diary-entry
		  :where (:and (:= 'claim-id 
				   (claim.claim-id claim))
			       (:= 'processed nil)))
		 1))))
	  (unless open-diary 
	    (insert-claim-diary 
	     claim (format nil "Claim still marked \"Open\" on ~A"
			   (print-date-to-string
			    (get-universal-time)))
	     :action-date (simple-date:universal-time-to-timestamp
			    (get-universal-time)))))))))

(define-descriptions diary-entry
 (:default
     (action-date :label "Due Date"))
  (:heading (claim-id :label "Diary for Claim #"
		      :activate ((h3)))
	    (user :activate (h4)))
  (:view
   (action-date)
    
   (note 
	 :label t)
   (defered :label "Defer date") 
   (processed :label t
	      ))
  (:heading (claim :label "Diary for"
		   :activate ((link-to-viewer) (h3)))
	    (user :activate (h4)))
  (:view
   (action-date )
    
   (note :activate ((<:pre :style "font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif; word-wrap:break-word; whitespace"))
	 :label t)
   (defered :label "Defer date")  
   (processed :label t))
  (:view-tab (tab :label "Deferred"
		  :tab deferred))
  (deferred 
      (defered
	  :as-table (:create defer-diary-entry diary-entry-id)
	:layers :diary-entry-tab))
  (:edit
   (action-date
		:type simple-date:date)
   (user 
    :edit nil) 
   (note :activate (pre)
	 :label t
	 :textarea (:rows 10))
   
   (processed :label t
	      :active t
	      :type cl:boolean))
  (:create-heading (:value 
		    :label "Create"
		    :value "Diary Entry"
		    :activate (h3)))
  (:create
   (claim-id :label "Claim"
	     :edit nil)
   (action-date :label "Date"
		:active t
		:type simple-date:date)
   (user :type app-user
	 :select-objects (list-diary-users)
	 :label t) 
   (note :activate ((blockquote) p)
	 :label t
	 :textarea (:rows 10)
	 :active t)) 
  (:overdue
   (view-a-btn
    :label nil
    :activate ((diary-link) (view-link)))
   (action-date :label "Date")
   (claim :label t
	  :activate (link-to-viewer)) 
   (note :activate ((blockquote) p)
	 :label t))

  (:claim
   (view-a-btn
    :label (:create diary-entry
		    claim-id)
    :activate ((diary-link) (view-link)))
   (action-date :label "Due Date")
   (user :layers :diary-entry
	 :attributes (person-or-username)
	 :label nil) 
   (note :activate ((blockquote) p)
	 :label t)
   (defered :label "Defer date")  
   (processed :label t))

  (:inline 
   (view-a-btn
    :label (:create diary-entry
		    claim-id)
    :activate  ((diary-link) (view-link)))
   (action-date :label "Date")
   (claim :label nil
	  :activate (link-to-viewer))

   (user :attributes ((username :label nil))
	 :label nil) 
   (note :activate ((blockquote) p)
	 :label t)
   (defered :label "Defer date")  
   (processed :label t)))



(define-descriptions defer-diary-entry 
  (:default 
   (diary-entry-id :label t)
   (defer-date :label t))
  (:diary-entry-tab
   (view-a-btn
    :label (:create defer-diary-entry
		    diary-entry-id)
    :activate (view-link))
   (defer-date :label t))
  (:create-heading 
   (:value :label "Diary Entry"
	   :value "Defer"
	   :activate ((h3))))
  (:create 
   (defer-date 
       :label "Defer Until"
     :type simple-date:date
     :active t)
   (diary-entry-id :edit nil))
  (:inline
   (defer-date :label nil))
  (:metaclass described-db-access-class))






	

      
  
