(in-package :maxclaims)

(define-description timecard (description-for-timecard)
  ((claim :validate (boundp))
   (date :input (:type simple-date))
   (minutes :label "# of Hours"
	    :input (:type number)
	    :value-formatter as-decimal
	    :validate (boundp))
   (notes :input (:type textarea) :value-formatter augment-newline-with-<br/>)
   (mileage-km :label "Mileage (km)"
	       :input (:type floating-point) 
	       :value-formatter as-decimal
	       :validate (boundp))
   (disbursements :value-formatter $-formatter
		  :input (:type currency)
		  :label "Disburse")
   (active-attributes
    :value '((claim :active :when
	      :attributes (claim-id insured)
	      :deactivate (editable))
	     (date)
	     (app-user :deactivate (editable) :attributes (username)) 
	     (minutes :active :when)
	     (mileage-km)
	     (disbursements)
	     (notes)))))

(define-layered-class timecard-totals-attribute (has-many-attribute) ())

(defaction create-new-interim (claim attribute)
  (with-described-object (claim (lol::attribute-description attribute))
    (let* ((related-object (rofl:make-object 
			    'timecard-interim)))
      (setf (slot-value related-object 'claim-id)
	    (slot-value claim 'claim-id))
      (create-object related-object))))

(defun timecard-totals-display (rows &key 
				       (number-of-td 3)
				       (title "TOTAL:")
				       (background-color "#EAEAEA")
				       (interim nil))
  (when (not (lol::unbound-slot-value-p rows))
    (<:tr :style (format nil "background-color: ~A" background-color)
	  (<:th (<:as-html title))
	  (when interim
	    (let ((a (find-attribute 'timecard-interim 'date)))
	      ;;(break "~A" (attribute-value a))
	     ;; (break "~A" interim)
	      (with-described-object (interim)
	      (<:td :style " white-space: nowrap;"
	       (<:as-html (display-attribute-value a)))))
	    (<:td 
	     (when (user-can-edit-p interim)
	       (<ucw:a :action (edit-object interim)
		       "(Edit)")))
	    (setf number-of-td (- number-of-td 2)))
     (dotimes (n number-of-td)
	    (<:td)) 
	  (<:td (<:as-html 
		 (as-decimal (loop for object in rows 
				   :if (slot-boundp object 'minutes)
				   :sum (timecard.minutes object)))))
	  (<:td (<:as-html 
		 (as-decimal (loop for object in rows 
				   :if (slot-boundp object 'mileage-km)
				   :sum (timecard.mileage-km object)))))
	  (<:td (<:as-html 
		 ($-formatter (loop for object in rows 
				    :if (slot-boundp object 'disbursements)
				    :sum (timecard.disbursements object))))))))


(defmethod display-has-many-table-rows ((attribute timecard-totals-attribute) object rows args)
  (apply 
   #'lol::funcall-with-described-object
   (lambda ()
     (let ((attributes (with-active-descriptions (inline)
			 (attributes *description*))))
       (<:thead 
	:style "background-color: #EEEEEE"
	(<:tr 
	 (<:th 
	  (unless (user-read-only-p $app-user)
	    (if ucw::*in-form*  
		(<ucw:submit :action (create-new-has-many object attribute)
			     "Create New") 
		(<ucw:form 
		 :action (create-new-has-many object attribute)
		 (<:submit :value "Create New"))))
	  (if ucw::*in-form*  
	      (<ucw:submit :action (create-new-interim object attribute)
			   "Add Interim") 
	      (<ucw:form :action (create-new-interim object attribute)
			 (<:submit :value "Add Interim"))))		    
	 
	 (when rows 
	   (dolist (a attributes)
	     (<:th (display-attribute-label a))))))  
       
       (when rows
	 (flet ((display-rows (rows)
		  (let ((interim 
			  (let ((rofl::*instance-is-persistent* t))
			    #+(or)(when (rofl::%ref-p 
				   (timecard.claim-id (first rows)))
			      (break "~A" (timecard.claim-id (first rows))))
			  (mapcar 
			   (lambda (p)
			     (make-object-from-plist 'timecard-interim p))
			     (postmodern:query 
			      (:order-by 
			       (:select '* 
				:from 'timecard-interim 
				:where (:and (:= 'claim-id 
						 (timecard.claim-id (first rows)))
					     (:>= 'timecard-interim.date
						  (timecard.date (first rows)))))
			       'timecard-interim.date)
			      :plists))))
			(totals nil))
		    (dolist (r rows)
		      (when (and interim 
				 (simple-date:time< 
				  (timecard-interim.date (first interim))
				  (timecard.date r)))
			(timecard-totals-display 
			 totals 
			 :title "Interim:"
			 :background-color "#EEEEEE"
			 :interim (first interim))
			(setf interim (cdr interim))
			(setf totals nil))
		      (push r totals)
			    
		    (with-described-object (r nil)
		      (<:tr (let ((object (attribute-object (first  attributes))))
			      (<:td 
			       (<ucw:a :action (view-object object)
				       (<:as-html "View"))
			       (when (user-can-edit-p object)
				 (<:ah " /  ")
				 (<ucw:a :action (edit-object object)
					 "Edit")))) 
			    (dolist (a attributes)
			      (with-attribute-context (a)
				(<:td (when (display-has-many-attribute-p attribute a)
					(with-active-descriptions (inline)
					  (display-attribute-value a))))))))

		      (when (and interim (eql r (car (last rows))))
			(timecard-totals-display 
			 totals 
			 :title "Interim:"
			 :background-color "#EEEEEE")
		)))))
			
	   (arnesi:if-bind page-size (page-size attribute)
	     (progn
	       (display-rows (subseq rows 0 page-size))
	       (<:tr (<:td
		      (let ((action
			      (make-action (lambda ()
					     (arnesi:with-call/cc
					       (call 'has-many-paged-list
						     :data rows
						     :page-size page-size
						     :offset 1
						     :object object
						     :args args
						     :attribute attribute))))))
			(if ucw::*in-form*
			    (<ucw:submit :action* action 
					 :value "View More")
			    (<ucw:form :action* action
				       (<:submit :value "View More"))))
		      )))
	     (display-rows rows)
	     )))))
   (when (consp rows) (first rows))
   nil
   (when rows args)))



(defvar *timecard-string*)
(defparameter *timecard-claim* nil)
(defparameter *timecard-user* nil)

(define-description user-timecards ())

(define-description link-to-timecards-viewer ()
  ())

(define-layered-method lol::display-using-description
  :in-layer #.(lol::defining-description 'link-to-timecards-viewer)
  :around (description d o &rest args)
  (let ((claim *timecard-claim*))
    (if (and *timecard-claim* 
	     (layer-active-p (defining-description 'html-description)))
	(with-inactive-descriptions (link-to-timecards-viewer)
	  (with-active-descriptions (inline)
	    (<ucw:a :action (view-user-timecards claim o) 
		    (call-next-method))))
	(call-next-layered-method))))

(define-layered-method lol::display-html-attribute (object (attribute timecard-totals-attribute))
  (let ((*timecard-string* nil))
    (let ((s 
	    (yaclml:with-yaclml-output-to-string 
	      (lol::display-html-attribute-value object attribute))))
      (<:tr 
       :class (format nil "~A lol-attribute" (lol::attribute-css-class attribute))
       (when (lol::attribute-dom-id attribute) 
	 :id (lol::attribute-dom-id attribute))
       (lol::display-html-attribute-label object attribute)
       (<:as-is s)))))

(define-layered-method display-attribute-label ((attribute timecard-totals-attribute))
   (let ((s *timecard-string*)
	 (u *timecard-user*))
     (<:as-html "Timecards:")
     (<:br)
     (<ucw:a :action (download-timecards-html attribute (funcall s) u)
	     (<:as-html "(download) " ))))



(defmethod display-has-many-table-to-string-function ((attribute timecard-totals-attribute) object rows args)
  (let ((tuser *timecard-user*))
    (dlambda ()
      (let ((*display* (make-instance 'component)))
	(declare (special *display*))
	(yaclml:with-yaclml-output-to-string 
	  (apply 
	   #'lol::funcall-with-described-object
	   (lambda ()
	     (<:table 
	      (<:style :type "text/css"
		       (<:as-html 
			"table {border-collapse: collapse;}
                   td {border: 1px solid grey; padding:5px;}
                   th {border: 1px solid grey;}"))
	      (let ((attributes (with-active-descriptions (inline)
				  (attributes *description*))))
		(<:h1 (<:as-html (format nil "Claim ~A Timecards"
					 (claim.claim-id object))
				 (when tuser
				   (format nil " for ~A" 
					   (app-user.username tuser)))))
		(<:thead :style "background-color: #EEEEEE"
			 (<:tr		    
			  (when rows 
			    (dolist (a attributes)
			      (<:th (display-attribute-label a))))))         
		(when rows
		  (flet ((display-rows (rows)
			   (dolist (r rows)

			     (with-described-object (r nil)
			       (<:tr  
				(dolist (a attributes)
				  (with-attribute-context (a)
				    (<:td (when (lol:attribute-active-p a)
					    (with-active-descriptions (inline)
					      (display-attribute-value a)))))))))))
		    (display-rows rows))))
	      (timecard-totals-display rows :number-of-td 2)))
	   (when (consp rows) (first rows))
	   nil
	   (when rows args)))))))
  
(defmethod display-has-many-table-rows :around ((attribute timecard-totals-attribute) object rows args)
  (when *timecard-user*
    (setf rows (remove-if-not (lambda (tc)
				(= (app-user.app-user-id *timecard-user*)
				   (timecard.app-user-id tc))) rows)))
  
  (let ((*timecard-claim* object))
    (setf *timecard-string* 
	  (display-has-many-table-to-string-function
	   attribute object rows args))
    (call-next-method)))
							      
(defun %dl-timecard-html (attribute string)
  (let* ((claim (attribute-object attribute))
	 (name (claim.claim-id claim))
	 (html-file (format nil "/tmp/~A-timecards.html" name))
	 (html-string    
	   (yaclml:with-yaclml-output-to-string 
	     (<:html (<:head (<:title (<:as-html (format nil "~A Timecards" name))))
		     (<:body (<:as-is string))))))
      
    (with-output-to-file (s html-file 
			    :if-exists :supersede
			    :if-does-not-exist :create)
      
      (princ html-string s))

    (values name html-file)))
    

(defaction download-timecards-html (attribute string user)
  (multiple-value-bind (name html-file) 
      (%dl-timecard-html attribute string)
    (download-file ;:content-type "applic"
		   :name (format nil "~A-~A~Atimecards.html" 
				 name 
				 (if user (app-user.username user) "")
				 (if user "-" ""))
		   :file html-file))) 
 
(defun as-decimal (ratio)
  (format nil "~$" ratio))

(defun augment-newline-with-<br/> (string)
  (prog1 "" 
    ;;HACK: skip over the AS-HTML escaping 
    ;;by writing directly to the stream.
    (and string (not (lol::unbound-slot-value-p string))
	 (loop :for char :across string 
	    :do (write-char char yaclml:*yaclml-stream*)
	    :if (eql char #\Newline)
	    :do (<:br)))))


(defmethod display-has-many-table-rows :after 
    ((attribute timecard-totals-attribute) object rows args)
  (timecard-totals-display rows))
