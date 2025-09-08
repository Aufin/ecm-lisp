(defpackage :maxclaims/ecm-description/app-user
  (:use :maxclaims/data-entity/app-user)
  (:export #:insert-app-user
	   #:update-app-user
	   #:app-user-person-or-username))

(in-package :maxclaims/ecm-description)

(defmethod insert-object ((object app-user))
  (maxclaims/ecm-description/app-user:insert-app-user object))

(defmethod update-object ((object app-user))
  (maxclaims/ecm-description/app-user:update-app-user object))

(defmethod object-attribute-value ((au app-user)
				   (a (eql 'person-or-username))
				   &key &allow-other-keys)
  (maxclaims/ecm-description/app-user:app-user-person-or-username
   au))

(define-descriptions maxclaims::app-user
  (:default
      identity username person)
  (:view
   (person :label "User is Person/Company"
	   :active :when
	   :activate (link-to-viewer)
	   )
   (login-history :label "Login")
   (can-edit :label "User can Edit?")
   (admin :label "User is Administrator?")
   (login :label "User can Login?")
   (password-disabled :label "Password"
		      :active t
		      :placeholder "No PW will disable this user"))
  (:diary-entry
   (person-or-username :layers :person-name))
  (:manage-viewer)
  (:manage-adjuster
   (view-a-btn
    :label (:create app-adjuster)
    :activate (view-link))
   (username :label t)
   (person :label t
	   :activate (link-to-viewer))
   (admin :label "User is Administrator?")
   (can-edit :label "User can Edit?"))
  (:manage
   (view-a-btn
    :label (:create app-user)
    :activate (view-link))
   (username :label t)
   (person :label t
	   :activate (link-to-viewer))
   (admin :label "User is Administrator?")
   (can-edit :label "User can Edit?"))
  (:person-tab
   (view-a-btn
    :label nil
    :activate (view-link))
   (username :label t)
   (admin :label "User is Administrator")
   (can-edit :label "User can Edit"))
  (:history
   (tab
    :label "Views"
    :tab
    :view-logs
    :active :when)
   #+(or)(tab
    :label "Messages"
    :tab :messages
    :active :when
    :active t)
   (tab
    :label "Searches"
    :tab
    :search-logs
    :active :when)
   (tab
    :label "Contracts"
    :tab :contracts-index
    :active :when)
   (tab
    :label "Agencies"
    :active :when
    :tab :agencies))
  (:diary    (tab
	      :label "Overdue Diaries"
	      :tab :overdue-diaries
	      :active :when))
  (:index
   (tab
	   :label "Views"
	   :tab
	   :view-logs
	   :active :when)
	  #+(or)(tab
	   :label "Messages"
	   :tab :messages
	   :active :when
	   :active t)
	  (tab
	   :label "Searches"
	   :tab
	   :search-logs
	   :active :when)
	  (tab
	   :label "Contracts"
	   :tab :contracts-index
	   :active :when)
    (tab
	   :label "Syndicates"
	   :active :when
	   :tab :syndicates)
	  (tab
	   :label "Agencies"
	   :active :when
	   :tab :agencies))
  (:messages
   (all-messages
    :label t
    :activate (link-to-viewer)))
  (:contracts-index
   (contracts
    :layers :app-user-tab
    :attributes (contract)))

  (:view-logs
   (view-logs :layers :default
	      :as-table t
	      :attributes (logged-object)))
  (:search-logs
   (search-logs :layers :search-logs
	      :attributes nil))
  (:create-heading (:value :label "Create"
			   :value "ECM User"
			   :activate ((h3))))
  (:heading (username
	     :label "User"
	     :activate ((h2 :style "display:inline-block"))))

  (:view-tab

   #+(or)(tab
    :label "Messages"
    :tab :messages
    :active :when)
   (tab
    :label "Contracts"
    :tab
    :contracts)
   (tab
    :label "Agencies"
    :tab :agencies
    )
   (tab
    :label "Syndicates"
    :tab :syndicates
    )
   (tab
    :label "Claims"
    :tab
    :claims)
    (tab
    :label "Overdue Diaries"
    :tab :overdue-diaries
    :active t))

  (:overdue-diaries
   (overdue-diaries :as-table t
		    :limit 25
		    :layers :overdue
		    :offset query-offset))
  (:claims
   (claims :as-table (:create app-user-claim
			      app-user-id)
	   :layers :inline))
  (:contracts
   (contracts :as-table (:create app-user-contract
				 app-user-id)
	      :layers :app-user-tab))

  (:syndicates
   (syndicates :as-table (:create app-user-syndicate
			      app-user-id)
	   :layers :app-user-tab))
  (:agencies
   (agency :as-table (:create app-user-agency
			      app-user-id)
	   :layers :app-user-tab))

  (:create (username :label "User Name"
		     :required t
		     :active t)
	   (password :label t
		     :active t
		     :placeholder "No PW will disable this user")
	   (person :label "User is Person"
		   :required t
		   :active t
		   :type person
		   :select-objects (:search maxclaims::string-search-person))
	   (can-edit :label "User can Edit?"
		     :active t
		     :type cl:boolean )
	   (admin :label "User is Administrator?"
		  :active t
		  :type cl:boolean)

	   (login :label "User can Login?"
		  :active t
		  :type cl:boolean))

  (:edit (username :label "User Name"
		   :required t
		   :active t)
	 (password :label t
		   :active t
		   :placeholder "No PW will disable this user")
	 (person :label "User is Person"
		 :required t
		 :active t
		 :type person
		 :select-objects (:search maxclaims::string-search-person))
	 (can-edit :label "User can Edit?"
		   :active t
		   :type cl:boolean )
	 (admin :label "User is Administrator?"
		:active t
		:type cl:boolean)
	 (login :label "User can Login?"
		:active t
		:type cl:boolean))
  (:inline (username :label "User")
	   (person :label "person")))

(defclass app-user-login ()
  ((total-logins :initarg :total-logins)
   (min-login-time :initarg :min-login-time)
   (max-login-time :initarg :max-login-time)))

(define-descriptions app-user-login
  (:default (total-logins :label "#")
      (min-login-time :label "first")
    (max-login-time :label "last"))
  (:inline (total-logins :initarg :total-logins)
      (min-login-time :initarg :min-login-time)
    (max-login-time :initarg :max-login-time)))

(defmethod object-attribute-value ((au app-user)
				   (a (eql 'login-history))
				   &key &allow-other-keys)
  (maxclaims/log/user-log:with-log-database (:app-user au)
    (apply #'make-instance 'app-user-login
	   (postmodern:query
	    "SELECT
             count(*) AS total_logins,
             MIN(log_time) AS min_login_time,
             MAX(log_time) AS max_login_time
      FROM user_log WHERE user_role = $1 AND log_type = 'LOGIN SUCCESS'
      GROUP BY user_role"
	    (app-user-rolename au)
	    :plist))))

(defmethod object-attribute-value ((au app-user)
				   (a (eql 'overdue-diaries))
				   &key (limit 50)
				     (offset 0))
  (maxclaims::query-objects
   'diary-entry
   (lambda (n fs)
     (let ((d.names
	    (loop :for f
	       :in (remove nil fs)
	       :collect `(:as ,(intern (format nil "D.~A" f)) ,f))))
       `(:limit
	 (:order-by
	  (:select
	   ,@fs :distinct
	   :from  (:as
		   (:select
		    ,@(list* '(:as  (:max dd.defer-date) defer-date)
			      d.names)
		     :from (:as ,n d)
		     :left-join (:as defer-diary-entry dd)
		     :on (:= dd.diary-entry-id
			     d.diary-entry-id)
		     :where
		     (:and (:not d.processed)
			   (:> (:now) d.action-date)
			   (:= app-user-id ,(app-user.app-user-id au)))
		     :group-by ,@(mapcar #'second d.names))

		   overdue)
	   :where (:or (:is-null defer-date)
		       (:> (:now) defer-date)))


	  action_date)
	 ,limit , (typecase offset
		    (symbol (funcall offset))
		    (T offset)))))))

(defmethod object-attribute-value ((au app-user)
				   (a (eql 'all-messages))
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::query-objects
    'app-user-message
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit
	(:order-by
	 (:select * :from 'app-user-message
	  :where (:or (:= from-id ,(app-user.app-user-id au))
		      (:= to-id ,(app-user.app-user-id au))))
	 (:desc app-user-message-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset))))))

(defmethod object-attribute-value ((au app-user) (a (eql 'password-disabled))
				   &rest args)
  (declare (ignore args))
  (let ((pw (maxclaims::app-user.password au)))
    (if (and pw (not (string= pw "")))
	"<HIDDEN>"
	"User is Disabled")))

(defmethod object-attribute-value ((au app-user) (a (eql 'view-logs))
				   &rest args)
  (declare (ignore args))
  (maxclaims/log/user-log:select-user-distinct-log
   au
   :log-type "view"))

(defmethod object-attribute-value ((au app-user) (a (eql 'search-logs))
				   &rest args)
  (declare (ignore args))
  (maxclaims/log/user-log:select-user-distinct-log
   au
   :log-type "search"))

(define-descriptions maxclaims/log/user-log::user-log
  (:default  maxclaims/log/user-log::log-info
      (logged-object :activate (link-to-viewer))
    (maxclaims/log/user-log::log-info
     :activate (search-link)))
  (:search-logs (maxclaims/log/user-log::log-info
     :activate (search-link)))
  (:inline ))

(defmethod object-attribute-value ((ul maxclaims/log/user-log::user-log)
				   (a (eql 'logged-object))
				   &rest args)
  (declare (ignore args))
  (maxclaims::find-object
   (intern (string-upcase
	    (substitute #\- #\_ (maxclaims/log/user-log::user-log.row-type ul)))
	   :maxclaims)
   (maxclaims/log/user-log::user-log.row-id ul)))


(define-descriptions app-adjuster
  (:default
      (view-a-btn
       :label (:create app-adjuster)
       :activate (view-link))
      (person :label t
	      :activate (link-to-viewer))
      (full-time :active t :label "Full Time")
      (active :active t :label "Active")
    (open-claim-count :label "Open Claims"))
  (:heading (:value :label nil
		    :value "Examiner"
		    :activate ((h4 :style "display:inline-block"))))
  (:view
   (person :label t
	   :activate (link-to-viewer))
   (full-time :label "Full Time")
   (active :label "Active")
   (open-claim-count :label "Open Claim Count"))
  (:view-tab
   (tab  :label "Open Claims"
	 :active :when
	 :tab open-claims-as-examiner))
  (open-claims-as-examiner
   (open-claims-as-examiner
    :as-table t
    :layers :person-tab
    :limit 25
    :activate (link-to-viewer)
    :offset query-offset))

  (:app-user-tab
   (view-a-btn))
  (:create-heading (:value :label "Create"
			   :value "Examiner"
			   :activate ((h3))))
  (:create
   (person
    :type person
    :select-objects (:search maxclaims::string-search-person))
   (full-time :label "Full Time"
	     :type boolean))
  (:edit

   (full-time :label "Full Time" :type boolean)
   (active :label "Active Examiner" :type boolean)
   (person
    :type person
    :select-objects (:search maxclaims::string-search-person)))
  (:inline person (full-time :label t) open-claim-count))

(define-descriptions app-user-contract
  (:default
      (view-a-btn
       :label (:create app-user-contract
		       app-user-id)
       :activate (view-link))
      (app-user-app-resource-id )
    app-user-id
    contract-id
    (contract :label t)
    (user :label t))
  (:heading (:value :label nil
		    :value "ECM User Contract (read only) for")
	    (user :active t
		  :attributes ((username)))
	    )
  (:view
   (user :label t
	 :activate (link-to-viewer))
   (contract :label t
	     :activate (link-to-viewer)))
  (:view-tab)
  (:app-user-tab
   (view-a-btn)
   (contract :label t
	     :activate (link-to-viewer)))
  (:create-heading (:value :label "Create"
			   :value "ECM User Contract (read only)"
			   :activate ((h3))))
  (:create
   (contract
    :required t
    :label t
    :active t
    :type contract
    :select-objects (:search maxclaims::string-search-contract))
   (user :label t :edit nil)
   (app-user-id :edit nil))
  (:inline (contract :label t) ))

(defmethod object-attribute-value ((au app-adjuster)
				   (a (eql 'open-claims-as-examiner))
				   &key (limit 25)
				     (offset 0))
  (maxclaims::filter-objects
   (maxclaims::query-objects
    'claim
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit
	(:order-by
	 (:select * :from claim
		  :where (:and (:= adjuster-id ,(maxclaims::app-adjuster.person-id au))
			       (:= status "Open")))
	 (:desc claim-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(defmethod object-attribute-value ((au app-adjuster)
				   (a (eql 'open-claim-count))
				   &rest args)
  (declare (ignore args))
  (postmodern:query
   (format nil "SELECT count(*) FROM
        (SELECT array_agg(claim_id)
          FROM claim RIGHT JOIN risk
          USING (risk_id)
         RIGHT JOIN policy USING (policy_id)
       WHERE adjuster_id = ~A AND status = 'Open'
      GROUP BY policy_id, date_of_loss) c;
" (maxclaims::app-adjuster.person-id au))
   :SINGLE))
(define-descriptions app-user-agency
  (:default
      (view-a-btn
       :label (:create app-user-agency
		       app-user-id)
       :activate (view-link))
      (app-user-agency-id
       app-user-id
       agency-id
       (agency :label t)
       (user :label t)))
  (:heading (:value :label nil
		    :value "ECM User Agency (read only) for")
	    (user :active t
		  :attributes ((username)))
	    )
  (:app-user-tab
   (view-a-btn)
   (syndicates :label t
	   :activate (link-to-viewer))
   (agency :label t
	   :activate (link-to-viewer)))
  (:view
   (user :label t
	 :activate (link-to-viewer))
   (agency :label t
	   :activate (link-to-viewer)))

  (:view-tab   )

  (:create-heading (:value :label "Create"
			   :value "ECM User Agency (read only)"
			   :activate ((h3))))
  (:create
   (agency
    :required t
    :label t
    :active t
    :type person
    :select-objects (:search maxclaims::string-search-person))
   (user :label t :edit nil)
   (app-user-id :edit nil))
  (:inline agency user))

(define-descriptions app-user-syndicate
  (:default
      (view-a-btn
       :label (:create app-user-syndicate
		       app-user-id)
       :activate (view-link))
      (app-user-syndicate-id
       app-user-id
       syndicate-id
       (syndicate :label t)
       (user :label t)))
  (:heading (:value :label nil
		    :value "ECM User Syndicate (read only) for")
	    (user :active t
		  :attributes ((username)))
	    )
  (:app-user-tab
   (view-a-btn)
   (syndicate :label t
	   :activate (link-to-viewer)))
  (:view
   (user :label t
	       :activate (link-to-viewer))
   (syndicate :label t
	            :activate (link-to-viewer)))

  (:view-tab)

  (:create-heading (:value :label "Create"
			   :value "ECM User Syndicate (read only)"
			   :activate ((h3))))
  (:create
   (syndicate
    :required t
    :label t
    :active t
    :type person
    :select-objects (:search maxclaims::string-search-person))
   (user :label t :edit nil)
   (app-user-id :edit nil))
  (:inline syndicate user))


(define-descriptions app-user-claim
  (:inline
   (view-a-btn
    :label (:create app-user-claim
		    app-user-id)
    :activate (view-link))
   (access :active t
	   :label "User has access?")
   (claim :label t
	  :activate (link-to-viewer)))
  (:default
      (view-a-btn
       :label (:create app-user-claim
		       app-user-id)
       :activate (view-link))
      (access :active t
	      :label "User has access?"))
  (:heading
	    (user :active t
		  :label "ECM User Claim Access for"
		  :attributes ((username))
		  :activate ((h3)))
	    )
  (:view
   (user :label t
	 :activate (link-to-viewer))
   (claim :label t
	  :activate (link-to-viewer))
   (access :active t
	   :label "User has access?"))

  (:view-tab   )

  (:create-heading (:value :label "Create"
			   :value "ECM User Claim Access"
			   :activate ((h3))))
  (:create
   (claim
    :required t
    :label t
    :active t
    :type claim
    :select-objects (:search maxclaims::string-search-claim))
   (access
    :label t
    :type boolean)
   (user :label t :edit nil)
   (app-user-id :edit nil))

  (:edit
   (claim
    :required t
    :label t
    :active t
    :type claim
    :select-objects (:search maxclaims::string-search-claim))
   (access
    :type boolean)
   (user :label t :edit nil)))
