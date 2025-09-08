(in-package :maxclaims)

(defclass timecard ()
  ((timecard-id :primary-key t)
   (claim-id :initarg :claim-id)
   (claim :column claim-id 
	  :references claim)
   (app-user :column app-user-id
	     :references app-user
	     :initform (or (ignore-errors $app-user) 1))
   (attachment :column attachment-id
	       :references attachment)
   (date 
    :initform (simple-date:universal-time-to-timestamp 
	       (get-universal-time)))
   (minutes :initform 0)
   (unbillable-hours :initform 0)
   (notes :initarg :notes)
   (mileage-km :initform 0)
   (disbursements :initform 0)
   app-user-id
   attachment-id)
  (:metaclass described-db-access-class))

(defun select-timecards (&key 
			   where			   
			   interim
			   parameters 
			   limit offset 
			   (filter #'maxclaims::filter-objects))
  ;(break "~A" where)
  (funcall filter
   (apply 
    #'maxclaims::query-objects 
    'timecard 
    (lambda (timecard fields)
      #+(or) ;; fields =>
      (TIMECARD
       (TIMECARD-ID CLAIM-ID DATE MINUTES NOTES MILEAGE-KM
		    DISBURSEMENTS APP-USER-ID))
      `(:raw 
	,(concatenate 
	 'string  
	 "WITH interim AS ("
	 (s-sql:sql-compile 
	   `(:union (:select 
		     (:as (:type :NULL integer) 
			  TIMECARD-ID)
		     (:as  timecard-interim.claim-id 
			   CLAIM-ID)
		     (:as timecard-interim.date DATE)
		     (:as (:sum timecard.MINUTES) minutes)
		     (:as (:sum timecard.unbillable-hours) unbillable-hours)
		     (:as (:type "Interim" text) NOTES)
		     (:as (:sum MILEAGE-KM)  MILEAGE-KM)
		     (:as (:sum DISBURSEMENTS) DISBURSEMENTS)
		     (:as (:type :NULL integer) APP-USER-ID)
		     (:as (:type :NULL integer) ATTACHMENT-ID)
		     :from 'timecard-interim
		     :left-join ,timecard
		     :on (:and 
			  (:= timecard-interim.claim-id
			      timecard.claim-id)
			  (:<= timecard.date 
			       timecard-interim.date))
		     :where ,where
		     :group-by timecard-interim.date
		     timecard-interim.claim-id)
		    (:limit
		     (:select
		      (:as (:type :NULL integer)
			   TIMECARD-ID)
		      (:as  timecard-interim.claim-id
			    CLAIM-ID)
		      (:as (:+ (:type "1 year" interval)
			       (:greatest (:max timecard-interim.date)
					  (:max timecard.date)))
			       DATE)
		      (:as (:sum MINUTES) minutes)
		      (:as (:sum unbillable-hours) unbillable-hours)
		      (:as (:type "Uninterim" text) NOTES)
		      (:as (:sum MILEAGE-KM)  MILEAGE-KM)
		      (:as (:sum DISBURSEMENTS) DISBURSEMENTS)
		      (:as (:type :NULL integer) APP-USER-ID)
		      (:as (:type :NULL integer) ATTACHMENT-ID)
		      :from 'timecard-interim
		      :left-join ,timecard
		      :on (:and 
			   (:= timecard-interim.claim-id
			       timecard.claim-id)
			   (:<= timecard.date 
				(:+ (:type "1 year" interval)
				    (:now))))
		      :where ,where
		      :group-by timecard-interim.date
		      timecard-interim.claim-id) 
		     1)))
	 ") "
	 (s-sql:sql-compile      
	  `(:limit 
	    (:order-by 
	     (:union 
	      (:select 
	       ,@fields
	       :from ,timecard
	       :where ,where)
	      ,@(when 
		 interim 
		 `((:select
		    (:as (:type :null integer) 
			 timecard-id)
		    i.claim-id
		    i.date
		    (:- i.minutes (:greatest (:max pre.minutes) 0))
		    (:- i.unbillable-hours (:greatest (:max pre.unbillable-hours) 0))
		    (:as i.notes notes)
		    (:- i.mileage-km (:coalesce (:max pre.mileage-km) 0))
		    (:- i.disbursements (:coalesce (:max pre.disbursements) 0))
		    i.app-user-id
		    (:as (:type :NULL integer) ATTACHMENT-ID)
		    :from (:as interim i)
		    :left-join (:as interim pre)		    
		    :on (:< pre.date 
			    i.date)
		    :group-by 
		    i.claim-id
		    i.date
		    i.notes
		    i.minutes
		    i.mileage-km
		    i.disbursements
		    i.app-user-id
		    i.unbillable-hours) 
		   ))
		 (:select 
		  (:as (:type :NULL integer) TIMECARD-ID)
		  (:as claim-id CLAIM-ID)
		  (:as :NULL DATE)
		  (:sum MINUTES)
		  (:sum unbillable-hours)
		  (:as "Total" NOTES)
		  (:sum MILEAGE-KM)
		  (:sum  DISBURSEMENTS)
		  (:as :NULL APP-USER-ID)
		  (:as (:type :NULL integer) ATTACHMENT-ID)
		  :from 'timecard
		  :where ,where
		  :group-by timecard.claim-id))
	 date)
	,limit 
	, (typecase offset
	    (symbol (funcall offset))
	    (T offset)))))))
    parameters)))


