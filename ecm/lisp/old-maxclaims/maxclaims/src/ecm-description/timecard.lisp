(defpackage :maxclaims/ecm-description/timecard 
  (:use)
  (:import-from :maxclaims
		#:select-timecards)
  (:export #:timecard-viewer
	   #:select-timecards))
(in-package :maxclaims/ecm-description)


(defun possible-timecard-attachments (claim-id)
  (let ((claim (maxclaims::find-object 'claim claim-id)))
    (maxclaims::claim.attachments claim)))
     
(define-descriptions timecard
  (:default
      (view-a-btn
       :label (:create timecard
		       claim-id)
       :activate (view-link))
      (maxclaims::date :label t
		       :type simple-date:timestamp
		       :layers :date-time
		       :active t)
    (app-user :attributes ((username :label nil)) 
	      :label "User")
    
    (notes :activate ((blockquote) 
			   (p :style "white-space:pre-wrap;"))
	   :label t)
    (minutes :active :when 
	     :label "Billable Hours")

    (unbillable-hours :active :when
		      :label "Unbillable Hours")
    (mileage-km 
     :label "Mileage") 
    (disbursements 
     :label "Disburse")
    (attachment
     :label "Attachment"
     :layers :timecard))
   (:create-heading 
    (:value :label "create" 
	    :value "Claim Timecard"
	    :activate ((h3 :style "display:inline-block")))
    (claim-id 
     :label "for claim #"
	       :activate ((h5 :style "display:inline-block"))))
   (:create
    (claim-id :label t 
	 :edit nil
	 :parse parse-integer)
    (date 
     :type simple-date:timestamp
     :layers :edit-date-time
     :active t)
    (notes :textarea (:rows 7 :cols 30)
	   :active t)
    (MINUTES :label "Billable Hours")

        (unbillable-hours :active :when
	     :label "Unbillable Hours")
    mileage-km
    disbursements
    (attachment :type attachment 
		:select-objects (:object possible-timecard-attachments 
					 claim-id)
		:layers :timecard))
  (:heading 
   (maxclaims::date 
    :label "Timecard on"
    :active t
    :activate ((h2 :style "display:inline-block")))
   (app-user :label "for"
	     :activate ((h3 :style "display:inline-block")))
   (claim :activate (h4 link-to-viewer)))
  (:view maxclaims::date 
	 app-user
	 (notes :activate ((blockquote) 
			   (p :style "white-space:pre-wrap;")))
	 (MINUTES :label "Hours")
	 (unbillable-hours 
	  :label "Unbillable Hours")
	 mileage-km
	 disbursements
	 attachment
)
  (:edit 
   (maxclaims::date 
    :active t
    :layers :date)
   (app-user :type app-user
	     :select-objects app-user)
   (notes :textarea (:rows 7 :cols 30))
   (MINUTES :label "Hours")
   (unbillable-hours :active :when
		     :label "Unbillable Hours")
       mileage-km
   disbursements
   (attachment :type attachment 
	       :select-objects (:object possible-timecard-attachments claim-id)
	       :layers :timecard))
  (:view-tab)
  (:inline
   #+(or)(view-a-btn)
   (maxclaims::date :label t
		    :active t)
   (APP-USER :ATTRIBUTES ((USERNAME :LABEL NIL)) 
	     :LABEL "User")
   
   (notes :activate ((blockquote) 
		     (p :style "white-space:pre-wrap;"))
	  :label t)
   (MINUTES :ACTIVE :WHEN
	    :label "Billable Hours")
   (unbillable-hours :active :when
		     :label "Unbillable Hours")
   (mileage-km ) 
   (disbursements :label "Disbursements")
   (attachment :layers :timecard))
  (:claim-tab
   (view-a-btn)
   (maxclaims::date :label t
		    :active t)
   (APP-USER :ATTRIBUTES ((USERNAME :LABEL NIL)) 
	     :active :when
	     :LABEL "User")
   
   (NOTES :th (:data-popout "true")
	  :td (:data-popout "true")
	  
	  :label t)
   (MINUTES :ACTIVE :WHEN
	    :label "Billable Hours"
	    )
   (unbillable-hours :active :when
		     :label "Unbillable Hours")
   (mileage-km) 
   (disbursements )
   (attachment :layers :timecard) ))
                   
(defmethod object-attribute-value ((tc timecard) (a (eql 'MINUTES))
				   &rest args)
  (declare (ignore args))
  (format nil "~1$" (call-next-method)))

(defmethod object-attribute-value ((tc timecard) (a (eql 'unbillable-hours))
				   &rest args)
  (declare (ignore args))
  (format nil "~1$" (call-next-method)))

(defmethod object-attribute-value ((tc timecard) (a (eql 'mileage-km))
				   &rest args)
  (declare (ignore args))
  (format nil "~$" (call-next-method)))

(defmethod object-attribute-value ((tc timecard) (a (eql 'disbursements))
				   &rest args)
  (declare (ignore args))
  (format nil "~$" (call-next-method)))

(defmethod object-attribute-value ((tc timecard) (a (eql 'app-user))
				   &rest args)
  (declare (ignore args))
  (let ((app-user (call-next-method)))
    (etypecase app-user
      (rofl::standard-db-access-object app-user)
      (null nil))))


(defclass maxclaims/ecm-description/timecard:timecard-viewer 
    ()
  ((interim 
    :initform t
    :initarg :interim
    :reader timecard-viewer-interim) 
   (where 
    :initarg :where
    :reader timecard-viewer-where
    :initform '(:= $1 claim-id))
   (parameters 
    :initarg :parameters
    :initform '(1234)
    :reader timecard-viewer-parameters)))

(define-descriptions maxclaims/ecm-description/timecard:timecard-viewer
    (:heading (:value :value ""))
    (:view
     (claims :label "Claim"
	     :activate (link-to-viewer))
     (info :label "Download"
		 :activate (download-timecard))
     (timecards 
      :as-table t))
    (:download
     (claims :label "Claim"))
    (:view-tab
     (tab :label "Timecards"
	  :tab :timecards))
    (:timecards (timecards 
		 :as-table t))
    (:default identity))

(defmethod object-attribute-value 
    ((tv maxclaims/ecm-description/timecard:timecard-viewer) 
     (a (eql 'info))
     &key &allow-other-keys)
  (cons (timecard-viewer-interim tv)
	(timecard-viewer-parameters tv))
  )

(defmethod object-attribute-value 
    ((tv maxclaims/ecm-description/timecard:timecard-viewer) 
     (a (eql 'claims))
     &key &allow-other-keys)
  (remove-duplicates 
   (mapcar #'maxclaims::timecard.claim
	   (object-attribute-value tv 'timecards))
   :key #'maxclaims::claim.claim-id))

(defmethod object-attribute-value 
    ((tv maxclaims/ecm-description/timecard:timecard-viewer) 
     (a (eql 'timecards)) 
     &key (limit 1000)
       (offset 0))
  ()
  (maxclaims/ecm-description/timecard:select-timecards
   :where (timecard-viewer-where tv)
   :parameters (timecard-viewer-parameters tv)   
   :interim nil   
   :limit limit 
   :offset offset))

(define-descriptions timecard-interim
  (:default
      (view-a-btn
       :label (:create timecard-interim
		       claim-id)
       :activate (view-link))
      (maxclaims::date :label t
		       :active t
		       :type simple-date:timestamp))
   (:create-heading 
	      (:value :label "create" 
		      :value "Claim Timecard Interim"
		      :activate ((h3 :style "display:inline-block")))
	      (claim-id 
	       :label "for claim #"
	       :activate ((h5 :style "display:inline-block"))))
   (:create
    (claim-id :label t 
	      :edit nil
	 :parse parse-integer)
    (maxclaims::date
     :type simple-date:timestamp
     :active t
     ))
  (:heading 
   (maxclaims::date 
    :label "Timecard Interim on"
    :activate ((h2 :style "display:inline-block")))
   (claim :activate (h4 link-to-viewer)))
  (:view maxclaims::date
	 claim-id)
  (:edit 
   (maxclaims::date :type simple-date:timestamp))
  (:view-tab)
  (:inline
   #+(or)(view-a-btn)
   (maxclaims::date :label t
		    :active t))
  (:claim-tab
   (view-a-btn)
   (maxclaims::date :label t
		    :active t)))

(defclass modify-timecards-change-claim-for-user ()
  ((user :initarg :user
	 :reader modify-timecards-change-claim-for-user-user)
   (from-claim 
    :initarg :from-claim
    :reader modify-timecards-change-claim-for-user-from-claim)
   (to-claim 
    :initarg :to-claim
    :reader modify-timecards-change-claim-for-user-to-claim)))

(defun users-from-claim-timecards (claim)
  (maxclaims::query-objects 
   'app-user
   (lambda (&rest _)
     (declare (ignorable _))
     `(:select 
       * 
       :from 'app-user
       :where (:in 
	       app-user-id
	       (:select 
		    'app-user-id 
		    :from 'timecard 
		    :where (:= claim-id ,
			       (maxclaims::claim.claim-id claim))))))))

(defmethod maxclaims/ecm-description::insert-object ((object modify-timecards-change-claim-for-user))
  (let ((user (modify-timecards-change-claim-for-user-user object))
	(from-claim (modify-timecards-change-claim-for-user-from-claim
		     object))
	(to-claim (modify-timecards-change-claim-for-user-to-claim
		   object)))
    (maxclaims::with-udb
      (postmodern:query
       (:update
	'timecard 
	:set 'claim-id to-claim
	:where 
	(:and 
	 (:= 'claim-id from-claim)
	 (:= 'app-user-id user)))))))
  
(define-descriptions modify-timecards-change-claim-for-user
  (:default )
  (:heading )
  (:view-report )
  (:view )
  (:view-tab )
  (:report )
  (:create (from-claim 
	    :label "From"
	    :active t
	    :edit nil)
	   (to-claim 
	    :label "To Claim #"
	    :active t
	    :type claim
	    :select-objects (:search maxclaims::string-search-claim :exact t)
	    :required t)

	   (user
	    :label t
	    :active t
	    :type person
	    :select-objects (:object users-from-claim-timecards from-claim)))
  (:create-heading (user :label "Move Timecards"
			 :activate (h3))))

(defmethod object-attribute-value ((tc modify-timecards-change-claim-for-user) (a (eql 'from-claim))
				   &rest args)
  (declare (ignore args))
  (maxclaims::find-object 'claim (call-next-method)))

(defmethod object-attribute-value ((tc modify-timecards-change-claim-for-user) (a (eql 'user))
				   &rest args)
  (declare (ignore args))
  (maxclaims::find-object 'app-user (call-next-method)))


      






	

      
  
