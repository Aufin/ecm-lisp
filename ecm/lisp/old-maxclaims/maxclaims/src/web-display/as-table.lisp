(defpackage :maxclaims/web-display/as-table
  (:use :cl :maxclaims/yaclml/tags)
  (:import-from :maxclaims/data-entity/app-user
		#:app-user-administrator-p)
  (:import-from :maxclaims/web-display/display
		#:*standard-objects-displayed*
		#:*current-tab*
		#:object-name-and-pkey-value
		#:activate-lambda
		#:<view-link
		#:<object-link
		#:<delete-link
		#:get-app-user
		#:display-as-table
		#:display
		#:create-a-btn)
  (:import-from :maxclaims 
		#:with-udb
		#:call-with-app-user))
(in-package :maxclaims/web-display/as-table)

(defmethod display-as-table ((object list) l 
			     &rest a &key as-table
				       (single NIL)
				       &allow-other-keys)
  (<:table 
   :class 
   (concatenate 'string "table " 
		(when object "table-striped table-bordered"))
  
   (unless (or object
	       (not (and (listp as-table) 
			 (eq (first as-table) :create)
			 (not  (maxclaims::user-read-only-p
				(get-app-user))))))
     (let ((access (object-name-and-pkey-value
		    (first *standard-objects-displayed*))))
       (create-a-btn 
	:create (princ-to-string (second as-table))
	:key (string-downcase (princ-to-string (third as-table)))
	:access (princ-to-string (car access))
	:id (princ-to-string (cdr access))
	:current-tab *current-tab*)
       ))
   (when object 
     (let ((atts (apply #'maxclaims/ecm-description::ecm-attributes 
			   (first object) l a)))
	 
       (<:thead 
	(<:tr 
	 (dolist (a atts)
	   (let ((label (maxclaims/ecm-description::attribute-label a)))
	   
	     (<:as-is 
	      (yasexml:<> `(th ,@(getf (rest a) :th))
		(yasexml:<> (:unescaped (yaclml:with-yaclml-output-to-string 
	    (if (and (listp label) 
		     (eq (first label) :create)
		     (not  (maxclaims::user-read-only-p
			    (get-app-user))))
		(let ((access (object-name-and-pkey-value
			       (first *standard-objects-displayed*))))
		      
		  (unless (and single object)
			
		    (create-a-btn 
		     :create (princ-to-string (second label))
		     :key (string-downcase (princ-to-string (third label)))
		     :access (princ-to-string (car access))
		     :id (princ-to-string (cdr access))
		     :current-tab *current-tab*)))
		(<:as-html (unless (listp label) label)))))))))))))
     (<:tbody
      (loop :for (o . rest) :on object
	 :do
	 (<:tr
	  (apply #'display o l :label nil  :as-table-row t a)))))))

(defmethod activate-lambda 
    ((name (eql 'maxclaims/ecm-description:view-link)))

  (lambda (o f &key edit) 
    (declare (ignore f))
    ;;    (when edit (break "E ~A" edit))
    (handler-case 
	      (let ((obj (slot-value o 'maxclaims/ecm-description::object)))
	        (flet ((v ()
		               (let ((access (object-name-and-pkey-value
				                          (first (last *standard-objects-displayed*)))))
		                 (when (and edit (maxclaims::user-can-edit-p obj))
		                   (<object-link
			                     (:object
			                      obj
			                      :place "edit"
			                      :tag-attributes
			                      (:class "btn btn-success")
			                      :uri-attributes
			                      `(("access[type]" ,(princ-to-string (car access)))
			                        ("access[id]" ,(princ-to-string (cdr access)))
			                        ,@(when *current-tab*
				                          `(,(list "back[active-tab]"
					                                 *current-tab*)))))
			                   (<:as-html "Edit"))
		                   (<:as-html " "))
		     
		     
		                 (<view-link
		                     (obj :class "btn btn-info"
			                        :target "_top")
		                   (<:as-html "view"))
		                 (when (app-user-administrator-p
			                      (get-app-user))
		                   (<:as-html " ")
		                   (<object-link
			                     (:object obj
				                    :place "delete"
				                    :tag-attributes
				                    (:class "btn btn-warning"
					                   :target "_top")
				                    :uri-attributes
				                    `(("access[type]" ,(princ-to-string (car access)))
				                      ("access[id]" ,(princ-to-string (cdr access)))				      ,@(when *current-tab* 					      															      `(("back[active-tab]" ,*current-tab*)))))
			                   (<:as-html "delete"))
		                   ))))
	          (typecase obj
	            (maxclaims::timecard
	             (if (ignore-errors
		                (maxclaims::timecard.timecard-id
		                 obj))
		               (progn (<:input :type "hidden"
				                           :name "timecard-id"
				                           :value (maxclaims::timecard.timecard-id obj))
		                      (v))
		               (<:form
		                :action "/ecm/timecard"
		                :method "GET"
		                :target "_top"
		                (<:input :type "hidden"
			                       :name "timecard[claim]"
			                       :value (maxclaims::timecard.claim-id obj))
		                (unless  (equalp "Total" (maxclaims::timecard.notes
					                                    obj))
		                  (<:input
		                   :type "hidden"
		                   :name "timecard[interim]"
		                   :value
		                   (ignore-errors
			                  (maxclaims/ecm-description::object-attribute-value
			                   (maxclaims::timecard.date
			                    obj)
			                   'maxclaims/ecm-description::date))))
		                (<:input
		                 :type "submit"
		                 :class "btn btn-inverse"
		                 :value (maxclaims::timecard.notes
			                       obj)))))
	            (t (v))))
		      
	        )
      (error (c)
	      (if maxclaims::*debug-on-error*
	          (error c)
	          nil)))))
