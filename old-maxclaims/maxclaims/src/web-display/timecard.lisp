(defpackage :maxclaims/web-display/timecard
  (:use :cl :maxclaims/yaclml/tags
	:maxclaims/web-display/display)
  (:import-from :maxclaims		
		#:call-with-app-user
		#:app-user
		#:with-ldb
		#:with-udb)
  (:import-from :maxclaims/data-entity/app-user
		#:app-user-read-only-p
		#:app-user-administrator-p)
  (:import-from :maxclaims/web-display/navbar
		#:navbar)
  (:import-from :maxclaims/web-display/html-page
		#:get-app-user
		#:with-user-html-page)
  (:import-from :maxclaims/web-display/view
		#:view-page)
  (:export #:timecard-page))

(in-package :maxclaims/web-display/timecard)

(defmethod activate-lambda 
    ((name (eql 'maxclaims/ecm-description:modify-timecard)))
  (lambda (o f) 
    (declare (ignore f))
    (let ((obj (slot-value 
		o 'maxclaims/ecm-description::object)))
      (<:a 
       :class "btn btn-warning"
       :href (format nil "create?create[type]=modify-timecards-change-claim-for-user&access[read-only]=false&attribute[from-claim]=~A" (maxclaims::claim.claim-id obj))
       "Change Claim for Users Timecards"))))

(defmethod activate-lambda 
    ((name (eql 'maxclaims/ecm-description:download-timecard)))
  (lambda (object f)
    (declare (ignore f))
    (<:a 
     :class "btn btn-success"
     :href 
     (format nil "/ecm/download-timecards?timecard[claim]=~A&timecard[interim]=~A"
	     (second object)
	     (or (first object) ""))
     (<:as-html "Download Timecards"))))



(defmacro <timecard-page (title &body body)
  `(with-user-html-page (:title ,title)
     (maxclaims::with-udb 
       ,@body)))

(defun timecard-page (&key
			(layers :view)
			navbar
			claim-id
			(interim-date)
		       offsets) 
  (<timecard-page 
      "ECM Timecards"
    (<:div       
     (view-page
      (make-instance 
       'maxclaims/ecm-description/timecard:timecard-viewer
       :where
       `(:and (:= $1 timecard.claim-id)
	      ,@ (unless (or (null interim-date)
			     (equal interim-date ""))
		  `((:<= timecard.date ,interim-date)
		    (:>= timecard.date
			 (:coalesce
			  (:limit (:select
				   (:max date)
				   :from timecard-interim
				   :where
				   (:and
				    (:= $1
					timecard-interim.claim-id)
				    (:< timecard-interim.date
					,interim-date)))
				  1)
			  "1900-01-01")))))
       :parameters (list claim-id)
       :interim interim-date)
      :tabs-inline t
      :layers layers
      :navbar navbar
      :offsets offsets))))
  

	  		


