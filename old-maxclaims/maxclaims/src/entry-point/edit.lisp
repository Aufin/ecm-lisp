(defpackage :maxclaims/entry-point/edit
  (:use :cl)
  (:import-from :maxclaims/entry-point/toplevel
		#:http-parameter-value
		#:http-parameters-as-alist
		#:http-parameters-changed-alist
		#:select-object-from-request
		#:object-typename-and-id-from-request)
  (:import-from :maxclaims/web-display/html-page
		#:get-app-user
		#:with-user-html-page)
  (:import-from :maxclaims/web-display/display
		#:<view-link)
  (:import-from :maxclaims
		#:with-udb
		#:call-with-app-user)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler
		#:redirect)
  (:import-from :maxclaims/ecm-description
		#:update-object
		#:ecm-attributes
		#:attribute-type)
  (:import-from :maxclaims/web-display/edit
		#:edit-page
		#:select-object-for-edit-page
		#:back-to-create/edit))
  

(in-package :maxclaims/entry-point/edit)

(defmacro with-app-user (() &body body)
  `(call-with-app-user 
    (get-app-user) 
    (lambda () 
      (with-udb
	,@body))))

(defun edit-page-handler ()
  (destructuring-bind (name id)
      (object-typename-and-id-from-request)
    (when (equalp name "claim")
      (hunchentoot:redirect
       (concatenate 'string "/ecm/claim/"
		    (princ-to-string id)
		    "/edit")
       :protocol ecm/hunchentoot:*protocol*)
      (return-from edit-page-handler nil))
    (when (equalp name "timecard")
      (hunchentoot:redirect
       (concatenate 'string "/ecm/timecard/"
		    (princ-to-string id)
		    "/edit"))
      (return-from edit-page-handler nil))

    (when (equalp name "timecard-interim")
      (hunchentoot:redirect
       (concatenate 'string "/ecm/interim/"
		    (princ-to-string id)
		    "/edit"))
      (return-from edit-page-handler nil))

#+(or)    (when (equalp name "diary-entry")
      (let ((things (hunchentoot:get-parameters*))
	    (possible-things '(diary-entry future[processed]
			       access[type] access[id]
			       go-back-to-claim)))
	#+(or)	(break "~w" (identity
			     (string-equal t (cdr (assoc 'future[processed] things
							 :test #'string-equal)))))
	(if
	 ;; If Mark as Done
	 (and (= (length things)
		 (length possible-things))
	      (let ((l (length things)) (foo t))
		(dotimes (n l foo)
		  (or (string-equal (car (nth n things))
				    (nth n possible-things))
		      (setf foo nil))))
	      (string-equal t (cdr (assoc 'future[processed] things
					  :test #'string-equal))))
	 (progn
	   (with-udb (ecm/entity/diary:update-diary-entry id :processed t))
	   (hunchentoot:redirect
	  (concatenate 'string "/ecm/diary-entry/"
		       (princ-to-string id)
		      ))
	   )
	 (hunchentoot:redirect
	  (concatenate 'string "/ecm/diary-entry/"
		       (princ-to-string id)
		       "/edit")))
	(return-from edit-page-handler nil)))

    (when (equalp name "risk")
      (hunchentoot:redirect
       (concatenate 'string "/ecm/risk/"
		    (princ-to-string id)
		    "/edit"))
      (return-from edit-page-handler nil))
    )
  (let* ((create-new (http-parameters-as-alist "create-new"))
	 (access-type (http-parameter-value "access[type]"))
	 (access-id (http-parameter-value "access[id]"))
	 (back[create] 
	  (when create-new 
	    (http-parameters-as-alist)))
	 (back (http-parameters-as-alist "back"))
	 (active-tab (cdr (assoc 
			   "active-tab" 
			   back :test #'string=)))
	 (object 
	  (ignore-errors 
	    (call-with-app-user 
	     (get-app-user) 
	     #'select-object-from-request)))
	 (go-back-to-claim (http-parameter-value "go-back-to-claim")))
    
    (when back[create]
      (let* ((edit-attribute 
	      (assoc (caar create-new) 
		     (ecm-attributes object :edit)
		     :test #'string-equal))
	     (edit-name (string-upcase
			 (attribute-type edit-attribute))))
	(return-from edit-page-handler
	  (maxclaims/entry-point/create::edit-page-handler 
	   (with-app-user ()
	     (apply #'make-instance 
		    (intern edit-name
			    :maxclaims/ecm-description)
		    nil)) 
	   :back back
	   :back-create (list* "edit" edit-name
			       back[create])
	   :file-attributes nil 
	   :cancel-link-fn  
	   (constantly  
	    (lambda ()
	      (back-to-create/edit  
	       nil back 
	       :back-create (butlast back[create])
	       :path "edit")))
		     
	   :changed nil
	   :create-key nil 
	   :access-type nil
	   :access-id nil ))))
    
    (if object
	(let ((present (http-parameters-as-alist "present")))
	  (if present 
	      (with-user-html-page ()
		(with-udb 
		  (loop for (name value) in present		    
		     :do  (setf 
			   (maxclaims/ecm-description::object-attribute-value 
			    object name)
			   (cond ((string-equal value "%:false:%")
				  NIL)
				 ((string-equal value ":NULL")
				  :null)
				 ((not (equalp value ""))
				  value)
				 (t :null)
				 )))
		  (update-object object)
		  (if (and active-tab)
		      (redirect 
		       (apply #'format nil "/ecm/view?~A=~A&tab[active]=~A"
			      (if go-back-to-claim
				  "claim"
				  access-type)
			      (if go-back-to-claim
				  go-back-to-claim
				  access-id)
			      active-tab))
		      (redirect (apply #'format nil "/ecm/view?~A=~A"
				       (if go-back-to-claim
					   (list "claim" go-back-to-claim)
					   (object-typename-and-id-from-request)))))
		  (<view-link object "done view?")))		     		     
	      (let ((changed (http-parameters-changed-alist "past" "future"))
		    (type-change (http-parameters-as-alist "type")))
		;;		(break "cha ~A : t ~A" changed type-change)
		(if type-change 
		    (with-user-html-page ()
		      (select-object-for-edit-page 
		       object  (first (first type-change))
		       changed
		       :search (http-parameters-as-alist "search")))
		    (edit-page 
		     object 
		     :back back
		     :go-back-to-claim go-back-to-claim
		     :access-type access-type
		     :access-id access-id
		     :inline-select? t
		     :changed-attributes 
		     (http-parameters-changed-alist "past" "future")
		     :future-attributes 
		     (http-parameters-as-alist "attribute"))))))	    
	(with-user-html-page ()
	  (<:as-html "Nothing to edit")))))

(define-easy-handler (|edit| :uri "/ecm/edit") 
    ()
  (edit-page-handler)
  )


  
