(defpackage :maxclaims/web-display/edit
  (:use :cl :maxclaims/yaclml/tags
	:maxclaims/web-display/display)
  (:import-from :maxclaims		
		#:call-with-app-user
		#:with-ldb
		#:with-udb
		#:with-adb
		#:with-db
		#:select-objects)
  (:import-from :maxclaims/ecm-description
		#:object-attribute-value
		#:ecm-attributes
		#:attribute-value)
  (:import-from :maxclaims/web-display/navbar
		#:navbar)
  (:import-from :maxclaims/web-display/html-page
		#:with-user-html-page)
  (:import-from :maxclaims/data-entity/app-user
		#:app-user-read-only-p
		#:app-user-administrator-p)
  (:export #:edit-page
	   #:edit-attribute-block
	   #:select-object-for-edit-page
	   #:back-to-create/edit))
(in-package :maxclaims/web-display/edit)

(defmacro with-edit-page (object title &body body)
  `(with-user-html-page (:title ,title) 
     (maxclaims::with-udb 
	   (navbar :object ,object
		   :active "Edit")
	   ,@body)))

(defun back-to-create/edit (object back 
			    &key back-create
			      (path "edit"))
#+ (or)  (break "back-to-create/edit 
Path: ~A 
object: ~A 
back: ~A
back-create: ~A" 
	 path object back back-create)
  (let* ((back-create-string 
	  (unless back-create
	    (cadr (assoc "create" back :test #'string=))))
	 (back-create 
	  (or back-create
	      (rest (rest  (when back-create-string
			     (let ((*readtable* (copy-readtable))
			    
				   (*read-eval* nil))
			       (setf (readtable-case *readtable*) :preserve)
			       (read-from-string back-create-string)))))))
	 (create-new 
	  (loop :for (name) :in back-create
		     :when (string-equal 
			    "create-new"
			    (subseq (string name) 0 
				    (position #\[ (string name))))
		     :do (return (subseq (string name) 
					 (1+ (position #\[ (string name)))
					 (position #\] (string name))))))
	 (back-create-inputs 
	  (progn 
		 ;; remove the create-new
		 (remove-if 
		  (lambda (acons)
		    (ignore-errors
		      (string-equal "create-new" 
				    (first acons)
				    :end1 10
				    :end2 10)))
		  
		  (remove 
		   nil 
		   (cons (when object 
			   (list 
			    (format nil "attribute[~A]" create-new)
			    (cdr (object-name-and-pkey-value object))))
			 (loop 
			    :for (name . value) 
			    :in 
			    (remove-if (lambda (acons)
					 (string-equal 
					  (format nil "future[~A]" create-new)
					  (car acons)))
				       (remove NIL back-create :key #'cdr))
			    :collect (cons
				      (cl-ppcre:regex-replace 
				       "future\\[" (string name) "attribute[")
				      value))))))))
    #+ (or)
    (break "back-create ~A 
create-new ~A 
back-create-inputs ~A" 
	   back-create create-new back-create-inputs)
						    
    (when back-create-inputs 
      (flet ((|go| ()
	       (<:form
		:method "POST"
		:action (format nil "~A?~{~{~A=~A~}~^&~}" 
				path
				(remove-if (lambda (acons)
					     (ignore-errors 
					       (string-equal 
						"attribute["
						(subseq (string  (first acons))
							0
							10))))
					   back-create-inputs)) 
		(loop :for (name val) :in back-create-inputs
		   :do (<:input :type "hidden" :name name :value val))
		(<:input :type "submit"
			 :class "btn btn-success"
			 :value "Go Back to Create"))))
	
	(if object  
	    (with-user-html-page (:title "Go Back?")
	      (<:h3 "Created:")
	      (maxclaims/web-display/display:display object :view)
	      (|go|))
	    (|go|))))))

(defun select/option-things ()
  #+(OR)(<:select 
		 :style "width:80%;"
		 :name (format nil "future[~A]" attribute-name)
		 :size 5
		 (when (and value (or changed-attributes (not create?)))
		   (<:option :value (cdr (object-name-and-pkey-value value))
			     :selected "selected"
			     (display value :inline)))
		 (when (getf (rest attribute) :allow-null)
		   (<:option :value ""
			     (<:as-html " ")))

		 (dolist (o (cond 
			      ((or (eq select-objects :search)
				   (and (listp select-objects) 
					(eq (first select-objects) :search)))
			       ;;(break "~A ~A"  select-objects search)
			       (funcall (second select-objects) 
					(second (assoc attribute-name
						       search :test 'string-equal))))
			      ((and (listp select-objects)
				    (eq :get-app-user (first select-objects)))
			       (list* (get-app-user)
				      (select-objects 'maxclaims::app-user)))
				 
			      
			      ((typep select-objects 'cons) 
			       (apply (first select-objects)
				      (rest select-objects)))
			      (t
			       (select-objects select-objects))))	       
		   (<:option :value (cdr (object-name-and-pkey-value o)) 
			     (display o :inline)))))


(defun select-object-for-edit-page (object attribute-name changed-attributes
				    &key (heading? :inline)
				      (form? t)
				      (layers :edit)
				      (create? nil)
				      (search nil))
  (<:div 
   :class "container"
   (with-udb
     (let* ((attribute (first 
			(maxclaims/ecm-description::ecm-attributes 
			 object layers 
			 :attributes  (list (typecase attribute-name
					      (symbol attribute-name)
					      (string (find-symbol
						       attribute-name 
						       :maxclaims/ecm-description)))))))
	    (select-objects (getf (rest attribute) :select-objects))
	    (changed-value #+ (or) (second  (assoc attribute-name changed-attributes
					   :test #'string-equal)))
	    (value (or changed-value (maxclaims/ecm-description::attribute-value attribute)))
	    (other-objects 
	     (cond 
	       ((and (listp select-objects) 
			 (eq (first select-objects) :object))
		(funcall (second select-objects)
			 (object-attribute-value object (third select-objects)))
		
		)
	       ((or (eq select-objects :search)
		    (and (listp select-objects) 
			 (eq (first select-objects) :search)))
		;;(break "~A ~A"  select-objects search)
		(funcall (second select-objects) 
			 (second (assoc attribute-name
					search :test 'string-equal))))
	       ((and (listp select-objects)
		     (eq :get-app-user (first select-objects)))
		(list* (get-app-user)
		       (select-objects 'maxclaims::app-user)))
	       ((typep select-objects 'cons) 
		(apply (first select-objects)
		       (rest select-objects)))
	       (t
		(select-objects select-objects)))))
      
       (when heading? 
	 (display object heading?)
	 (<:h3 
	  :style "display:inline-block;" 
	  (<:as-html ": Select " (maxclaims/ecm-description::attribute-label attribute))))
       ;; * The Radio Buttons
       
       (flet ((doit (&key (class "span10"))
		;; ** Checked Radio Value
		;; (display value :inline)		
		(let ((checked (and value 
				    (or (not (stringp value))
					(not (string-equal value "None")))		    
				    (or changed-attributes (not create?))))
		      ;; ** Allow Null
		      (allow-null (getf (rest attribute) :allow-null)))
		(<:div 
		 :class class
		 :style "max-height:10em;overflow:auto; 
                        border:1px dotted grey;margin:2px;margin-bottom:10px"
		
		 ;; ** When Checked
		 (when checked
		   (<:label 
		    :class "radio"
		    (<:input :type "radio"
			     :checked "checked"
			     :name (format nil "future[~A]" attribute-name)
			     :value (or (and (equal value "") "")
					(cdr (object-name-and-pkey-value value))
					" "))
		    (<:span (display value :inline))))
		 ;; ** When Allow null
		 (when allow-null
		   (<:label 
		    :class "radio"
		    (<:input :type "radio"
			     :name (format nil "future[~A]" attribute-name)
			     :value ":null"
			     :checked (unless checked
					(setf checked t)))
		    (<:span (<:as-html "Nothing"))))
		 
		 ;;;; ** This seems to be the Radio button place for everything else
		 (dolist (o other-objects)
		   (<:label 
		    :class "radio"
		    (<:input :type "radio"
			     :checked (unless checked
					(setf checked t))
			     :name (format nil "future[~A]" attribute-name)
			     :value (or (cdr (object-name-and-pkey-value o))
					 ""))
		    (<:span (display o (or (getf (rest attribute) :layer)
					   (getf (rest attribute) :layers)
					   :inline)))))))))
	 (if (not form?)
	     (doit)
	     (<:form 
	      :method "POST"
	      :action "#"
	      (<:div 
	       (doit :class ""))	      
	      (dolist (a changed-attributes)
		(let ((name (first a))
		      (value (second a)))
		  (<:input 
		   :type "hidden"
		   :name (concatenate 
			  'string "future[" (princ-to-string name) "]")
		   :value value)))
	      (<:hr)
	      (<:div :class "container"
		     :style "display:block"
	       (<:button 
		:class "btn btn-success"
	       :type "submit"
	       (<:as-html "Select")
	       )
	       (unless (app-user-read-only-p (get-app-user))
		 (<:button 
		  :name (format nil "create-new[~A]" attribute-name)
		  :class "btn btn-warning"
		  :type "submit"
		  (<:as-html "Create New")))))
	     ))))))

(defun edit-attribute-block (object 
			     attribute
			     past-object-attributes
			     changed-attributes 
			     future-attributes
			     inline-select?
			     layers)
  (let* ((label (maxclaims/ecm-description::attribute-label attribute))
	 (name (maxclaims/ecm-description::attribute-name attribute))
	 (value (maxclaims/ecm-description::attribute-value attribute))
	 (required (maxclaims/ecm-description:attribute-required attribute))
	 (future-value (second (assoc name future-attributes
				      :test #'string-equal)))
	 (type (maxclaims/ecm-description::attribute-type attribute))
	 (edit? (getf (rest attribute) :edit t))
	 (prepend (getf (rest attribute) :prepend))
	 (placeholder (getf (rest attribute) :placeholder))
	 (select-objects (getf (rest attribute) :select-objects))
	 (past-value (attribute-value  
		      (find name past-object-attributes 
			    :key #'car :test #'string-equal))))
	     
    (<:div 
     :class "row-fluid"
     (<:div 
      :class "span2"
      (<:span :class "label"
	      (<:as-html label)))
     (<:div 
      :class "span10"
      #+ (or)  (when past-value)
      (when (and (not edit?) value
		 (not (integerp 
		       (ignore-errors 
			 (parse-integer  value)))))
	(display value :inline))
      (<:input 
       :type "hidden"
       :name (concatenate 
	      'string "past[" 
	      (princ-to-string name) "]")
       :value (cond ((string-equal type 'timestamp)
                     (maxclaims/text-display:display past-value :date-time))
                    ((string-equal type 'date)
                     (maxclaims/text-display:display past-value :date))
                    (t 
                     (object-html-value past-value))))

      (when changed-attributes 
	(<:input 
	 :type "hidden"
	 :name (concatenate 
		'string "present[" 
		(princ-to-string name) "]")
	 :value (if (string-equal type 'timestamp)
		    (if value 
			(maxclaims/text-display:display value :inline)
			"")
		    (or (object-html-value value) "")))
	
	(when value 
	  (apply
	   #'display value :inline 
	   :attributes (getf  (rest attribute) :attributes)
	   :label nil 
	   :activate (getf  (rest attribute) :activate)
	   (rest attribute))))
      ;; (<:as-html "type" type "s " (princ-to-string inline-select?))      
      (if (or changed-attributes 
	      type)
	  (if (and (or  (string-equal type 'timestamp)
			(string-equal type 'date))
		   (not changed-attributes))

	      ;; the date picker
	      
	      (<:as-is 
	       '#:|<div class="input-append date |
	       (if (string-equal type 'timestamp)
		   "form_datetime"
		   "form_date")
	       '#:|">
                    <input size="16" type="text" value="|
	       (or (and value 
			(maxclaims/text-display:display 
			 value 
			 (if (string-equal type 'timestamp)
			     :date-time
			     :date)))
		   "")
	       '#:|" name="| 
	       (concatenate 
		'string "future[" (princ-to-string name) "]")
	       '#:|">|
	       '#:|<span class="add-on"><i class="icon-th"></i></span>
                   </div> |)
	      #+NIL(<:as-is 
	       '#:|<input type="text" class="date-pckr valid |
	       (when (and (not value) required) "input-required")
	       "\" "
	       (if (and (not value) required)
		   '#:|placeholder="Required" |
		   "")
	       '#:|value="|
	       (or (and value (maxclaims/text-display:display value :inline))
		   "")
	       '#:|" name="| 
	       (concatenate 
		'string "future[" (princ-to-string name) "]")
	       '#:|" data-date-format="yyyy-mm-dd" data-date-autoclose="true">|)
		       
	      (if (and inline-select?
		       (not changed-attributes)
		       (not (or (eq select-objects :search)
				(and (listp select-objects) 
				     (eq (first select-objects) :search)))))
		  (progn  
		    (if (eq type 'cl:boolean)
			(if (object-html-value value)
			    (progn
			      (<:input 
			       :type "hidden"
			       :name (concatenate 
				      'string "future[" 
				      (princ-to-string name) "]")
			       :value "%:false:%")
			      (<:input 
			       :type "checkbox"
			       :name (concatenate 
				      'string "future[" 
				      (princ-to-string name) "]")
			       :checked "checked"
			      ))
			    (<:input 
			       :type "checkbox"
			       :name (concatenate 
				      'string "future[" 
				      (princ-to-string name) "]")
			       :value nil))
			
			(select-object-for-edit-page 
			 object name 
			 (append changed-attributes future-attributes) 
			 :layers layers
			 :heading? nil
			 :form? nil
			 :create? t)))	
		  (<:div 
		     
		   (let ((changed-value (second (assoc name changed-attributes  
						       :test #'string-equal))))
		     #+ (or) (when changed-value 
			       (<:input 
				:type "hidden"
				:name (format nil "future[~A]" name)
				:value (object-html-value changed-value)))

		     (when (and value (not changed-attributes)) 
		       (<:input 
			:type "hidden"
			:name (concatenate 
			       'string "future[" 
			       (princ-to-string name) "]")
			:value (if (string-equal type 'timestamp)
				   (maxclaims/text-display:display value :date-time)
				   (object-html-value value)))
		       (when value 
			 (or (ignore-errors
			       (prog1 t 
				 (<view-link value 
				   (apply
				    #'display value :inline 
				    :attributes (getf  (rest attribute) :attributes)
				    :label nil 
				    :activate (getf  (rest attribute) :activate)
				    (rest attribute)))))
			     (<:as-html (if (string-equal type 'timestamp)
					    (if value 
						(maxclaims/text-display:display value :date-time)
						"")
					    (or (object-html-value value) "")))))
		       )
			   
		     (when (and (not changed-attributes)
				(or (eq select-objects :search)
				    (and (listp select-objects) 
					 (eq (first select-objects) :search))))
		       (<:br)
		       (when (and (listp select-objects)
				  (member :existing select-objects)
				  (eq (class-name (class-of (class-of object))) 
				      'maxclaims::described-db-access-class))
			 (let ((slotd (find name  
					    (c2mop:class-slots 
					     (class-of object))
					    :test (lambda (x y)
						    (string-equal 
						     (string x)
						     (c2mop:slot-definition-name y)))))
			       (table-name (rofl::class-table-name (class-of object))))
			   (when slotd
			     (<:select :name (concatenate 
					      'string "future[" (princ-to-string name) "]")
				       :style "width:80%"
			      (<:option :disabled t :selected t
					(<:as-html "Select Existing: "))
			      (mapcar (lambda (e)
					(<:option 
					 :value (cdr (object-name-and-pkey-value e))
					 (<:as-html (maxclaims/text-display:display
						     e :select-option))))
				       (maxclaims::existing 
					table-name 
					(rofl::slot-definition-column-name slotd)
					(rofl::slot-definition-foreign-type slotd)))))
			   (<:br)(<:b (<:as-html " or try "))))
		       (<:input 
			:class (concatenate 'string  "search" 
					    (if (and required
						     (not value))
						" input-required" ""))
			:type "text"
			:style "width:80%"
			:placeholder (concatenate 
				      'string
				      " "
				      (princ-to-string label)
				      " Search / Create" 
				      (if (and required
					       (not value)) "(required)" "")) 
			:name (format nil "search[~A]"
				      name)))
				  
		     (when (and type (not changed-attributes))
		       (<:input 
			:type "submit"
			:class "btn btn-mini"
			:name (format nil "type[~A]"
				      name)
			:value  "Search"))))))

	  (flet ((doit (&optional (textarea (getf (rest attribute) :textarea)))
		   (let* ((input (getf (rest attribute) :input))
			  (input-type (getf input :type))
			  (iname (concatenate 
				  'string "future[" (princ-to-string name) "]"))
			  (value (or (first (rest (assoc name future-attributes
							 :test #'string-equal)))
				     value)))
		     (if textarea
			 (<:textarea 
			  :class "field span8"
			  :name iname
			  :rows (getf textarea :rows 5)
			  :cols (getf textarea :cols 5)
			  (when value (<:as-html value))) 
			 (progn 
			   (when (string-equal input-type "file")
			     (<:input 
			      :type "hidden"
			      :name (concatenate 
				     'string "file[" 
				     (princ-to-string name)"]")))
			   (<:input 
			    :class (when required "input-required")
			    :placeholder (if required "Required" placeholder)
			    :type (or input-type
				      (if edit? "text" "hidden"))
			    :name iname
			    :value (object-html-value  value)))))))
	    (if prepend
		(<:div 
		 :class "input-prepend"
		 (<:span :class "add-on"
			 (<:as-html prepend))
		 (doit))
		(doit))
	    
	    (unless (or edit? changed-attributes)
	      (<:span :class "input-xlarge uneditable-input"
		      (display (or (first (rest (assoc name future-attributes
						       :test #'string-equal)))
				   (object-html-value value)) :inline)))))))))

(defun edit-page (object 
		  &key 
		    go-back-to-claim
		    back
		    back-create
		    changed-attributes
		    future-attributes
		    (access-type)
		    (access-id)
		    (layers :edit)
		    (inline-select? nil)
		    (heading :heading)
		    (submit-text "Submit Edit")
		    (success-text "Edit using these values")
		    (cancel-link nil)
		    (condition nil))
  (with-edit-page 
      object
      (with-adb 
	(concatenate 
	   'string "ECM Edit:" 
	   (maxclaims/text-display:display object heading)))
    	(<:script
	 :type "text/javascript"
	 (<:as-is
	  '|$('button').click(function(){
   	     $('p').text("Form submiting.....").addClass('submit');
	     $('button').attr("disabled", true);	
       }); |)
		  )
    (when condition 
      (<:div 
       :class "text-error"
       (<:h1 (<:as-html "Error:"))
       (<:pre 
	(<:as-html (princ-to-string condition)))))
    
    (let* ((active-tab (second (assoc 
				"active-tab" 
				back :test #'string=)))
	   (past-object-attributes
	    (with-adb  
	      (ecm-attributes 
	       object layers)))
	   (future-object-attributes
	    (with-adb  
	      (loop 
		 :for (name val) 
		 :in (append future-attributes changed-attributes)
		 :do 
		 (when name 
		   (let* ((pa (assoc name past-object-attributes 
				    :test #'string-equal))
			  (type (maxclaims/ecm-description::attribute-type pa)))
		     
		     #+ (or) (break "Seeting ~A  to ~S ~%~A :
type : ~A
 ~A" name val pa type
 
 (append future-attributes changed-attributes))
					;		     (break "~A ~A ~A" name object val)
		     (setf (object-attribute-value 
			    object name)
			   (if (string-equal val ":null")
			       :null
			       (if type 
				   (or (and (not (equalp val "")) val) 
				       :NULL)
				   val)))))
		 :finally (return (ecm-attributes 
				   object layers)))))
	    
	   (as (if changed-attributes 
		   (loop for (n v) in changed-attributes 
		      collect (assoc n future-object-attributes :test #'string-equal))
		   future-object-attributes)))
      
     ;; (break "~A" object)
      (<:div 
       :class "container-fluid"
       (<:div 
	:class 
	"container"
	(display object heading)
	(<:hr) 
	(flet ((form-buttons ()
		 (when go-back-to-claim
		   (<:input :type "hidden"
			    :name "go-back-to-claim"
			    :value (string-left-trim
				    " "
				    (princ-to-string go-back-to-claim))))
		 
		 
		 (if changed-attributes 
		     (progn 
		       (<:button 
			:class "btn btn-success"
			:data-loading-text "Loading..."
			:type "submit"
			(<:as-html success-text))
		       (when active-tab 
			 (<:input :type "hidden"
				  :name "access[type]"
				  :value access-type)
			 (<:input :type "hidden"
				  :name "access[id]"
				  :value access-id)
			 (<:input :type "hidden"
				  :name "back[active-tab]"
				  :value active-tab)))
		     (<:button 
		      :style "display:inline-block;"
		      :class "btn btn-success"
		      :type "submit"
		      (<:as-html submit-text))))
	       (non-form-buttons ()
		 (when changed-attributes 
		   (<:form 
		    :style "display:inline-block;"
		    :action "#"
		    :method "POST"
		    (dolist (a as)
		      (let ((name (maxclaims/ecm-description::attribute-name a))
			    (value (maxclaims/ecm-description::attribute-value a)))
			(<:input 
			 :type "hidden"
			 :name (concatenate 
				'string "attribute[" (princ-to-string name) "]")
			 :value 
			 (typecase value 
			   (maxclaims::risk-type 
			    (maxclaims::risk-type.type-name value))
			   (maxclaims::standard-db-access-object 
			    (cdr (object-name-and-pkey-value value)))
		  
			   (t  value)))))
		    (<:button 
		     :class "btn btn-warning"
		     :style "display:inline-block;margin-left:1em; "
		     :type "submit"
		     (<:as-html "Go back to editor"))))
	
		 ;;(break "~A" (maxclaims::claim.ibc-code object))
		 (etypecase cancel-link
		   (string 	    
		    (<:a :class "btn btn-danger" 
			 :style "display:inline-block;margin-left:1em; "
			 :href cancel-link
			 (<:as-html "Cancel Edit")))
		   (function (funcall cancel-link))
		   (null 	    
		    (<view-link (object :class "btn btn-danger"
					:style "display:inline-block; 
                                       margin-left:1em;") 
		      
		      "Cancel Edit")))))
	  (<:div :style "float:right"
		 (non-form-buttons))
	  (<:form 
	   :id "edit-record"
	   :name "edit-record"
	   :style "display:inline;"
	   :action 
	   (if back-create
	       (format 
		nil
		"create?create[type]=~A&access[read-only]=false"
		(second back-create))
	       "#")
	   :method "POST"
	   :enctype "multipart/form-data"
	   (form-buttons)
	   (when back-create
	     (<:input :type "hidden"
		      :name "back[create]"
		      :value (prin1-to-string back-create)))
	   (<:hr)

	   
	   (dolist (a as)
	     (edit-attribute-block 
	      object 
	      a 
	      past-object-attributes
	      changed-attributes 
	      future-attributes
	      inline-select?
	      layers))
	   (<:hr)
	   (form-buttons))
	  (<:div :style "float:right" (non-form-buttons)))
	)))))
