(defpackage :maxclaims/web-display/report
  (:use :cl :maxclaims/yaclml/tags
	:maxclaims/web-display/display)
  (:import-from :maxclaims		
		#:call-with-app-user
		#:with-ldb
		#:with-udb
		#:object-table-name
		#:object-id
		#:app-user-log)
  (:import-from :maxclaims/ecm-description
		#:ecm-attributes
		#:attribute-label
		#:attribute-value)
  (:import-from :maxclaims/web-display/navbar
		#:navbar)
  (:import-from :maxclaims/web-display/html-page
		#:with-user-html-page
		#:get-app-user)
  (:export #:bordereau-report-page))
(in-package :maxclaims/web-display/report)

(defmethod activate-lambda 
    ((n (eql 'maxclaims/ecm-description:contract-bordereau)))
  (lambda (o f) 
    (declare (ignore f))
    (let ((obj (slot-value 
		o 'maxclaims/ecm-description::object)))
      (<:a 
       :class "btn"
       :href (format nil "/ecm/report/bordereau/casualty-liability?contract-id=~A" 
		     (maxclaims::contract.contract-id obj))
       "View Bordereau"))))

(defun bordereau-field (field)
  (<:td  
   (typecase field 
     (string (<:as-html field))
     (vector (<:div 
	      (loop :for row across field
		 :do (<:div  :style "margin:1em; padding:0.5em ;border:1px solid grey;"
			     (dolist (l row)
			       (<:span :style (format nil "padding-right:1em;width=~A" (coerce (/ 100 (length row)) 'integer))
				       (display l :inline)))))))
     (t 
      (unless (eq :null field)
	(display field :inline))))))

(defun bordereau-row (line)
  
  (<:tr 
   (loop :for field in line
      :do
      (<:td  
       (bordereau-field field)))))

(defun bordereau-report-page (bordereau-report
			      &key object
				(line-fn #'identity))
 
  (with-user-html-page ()
    (let ((yaclml:*yaclml-indent* t))
       (when object 
          (<:div 
       :class "container"
       (setf (maxclaims/ecm-description::bordereau-report-risk-type
	      object)
	     (remove :null 
		     (remove-duplicates (mapcar #'cdr (mapcar 'first bordereau-report))
					:test #'string-equal)))
       (display object :heading)
       (<:hr)
       (<:div 
	:class "container-fluid"
	(let ((as (maxclaims/ecm-description::ecm-attributes 
		   object :view)))
	  (<:table
	   :class "table table-bordered"
	   (dolist (a as)
	     (let ((label (maxclaims/ecm-description::attribute-label a))
		   (value (maxclaims/ecm-description::attribute-value a))
		   (alayers (maxclaims/ecm-description::attribute-layers a))
		   (active (maxclaims/ecm-description::attribute-active a)))
	       (when (not (and (eq :when active)
			       (null value)))
		 (<:tr (<:td (<:span :class "label"
				     (<:as-html label))) 
		       (<:td 
			(<:span 
			 :class "attribute"
			 (apply #'display value alayers 
				:attributes (getf  (rest a) :attributes)
				:label nil 
				:activate (getf  (rest a) :activate)
				(rest a))
			 (<:as-html " "))))))))))))
       
       (if (maxclaims/ecm-description::bordereau-report-risk-type object)
	   (setf bordereau-report 
		    (loop :for type :in (maxclaims/ecm-description::bordereau-report-risk-type object)
		       :collect (remove-if-not (lambda (row)
						 (equalp (cdr (first row)) type)) bordereau-report)))
	   (setf bordereau-report 
		 (list bordereau-report)))

       (<:table               
	(dolist (bordereau-report bordereau-report)
	  (<:tr
	   (<:td 
	    (<:table 
	     :class "table table-bordered"
	     (<:tr 
	      (<:th :colspan 
		    (length (funcall line-fn (first bordereau-report)))
		    (<:b (<:as-html (cdar (first bordereau-report))))))
	     (<:tr 
	      (dolist (f (funcall line-fn (first bordereau-report)))
		(<:th
		 :style "background-color:#FFFEAB;"
		 (maxclaims/web-display/display:display (car f) :inline ))))
	     (dolist (line bordereau-report)
	       (<:tr 
		(loop :for (nil . field) :in (funcall line-fn line)
		   :do
		   (<:td  
		    (typecase field 
		      (string (<:as-html field))
		      (vector (<:div 
			       (loop :for row across field
				  :do (<:div  :style "margin:1em; padding:0.5em ;border:1px solid grey;"
					      (dolist (l row)
						(<:span :style (format nil "padding-right:1em;width=~A" (coerce (/ 100 (length row)) 'integer))
							(display l :inline)))))))
		      (t 
		       (unless (eq :null field)
			 (display field :inline))))
		    ))))))))))))
