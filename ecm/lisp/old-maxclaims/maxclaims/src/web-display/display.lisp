(defpackage :maxclaims/web-display/display
  (:use :cl :maxclaims/yaclml/tags)
  (:import-from :maxclaims/ecm-description
		#:ecm-attributes)
  (:import-from :maxclaims/data-entity/app-user
		#:app-user-administrator-p)
  (:import-from :maxclaims 
		#:with-udb
		#:call-with-app-user
		#:user-can-edit-p
		#:user-read-only-p
		#:user-is-adjuster-p)
  (:import-from :max-ecm/gnumeric/ssconvert
		#:ssconvert-type
		#:ssconvert-type-value)
  
  (:export	#:*standard-objects-displayed* 
		#:get-app-user
		#:display
		#:<view-link
		#:<edit-link
		#:<delete-link
		#:<search-link
		#:object-name-and-pkey-value
		#:object-html-value

		;; * Activate
		#:activate-lambda

		#:display
		#:display-as-table
		))
(in-package :maxclaims/web-display/display)

(defvar *view-name-and-pkey-value* NIL)

(defun get-app-user ()
  (maxclaims/hunchentoot:session-value :app-user))

(defun object-html-value (object)
  (typecase object
    (simple-date:timestamp 
     (maxclaims/text-display:display object :inline))
    (maxclaims::risk-type 
     (maxclaims::risk-type.type-name object ))
    (maxclaims::standard-db-access-object 
     (cdr (object-name-and-pkey-value object)))		  
    (t  object)))

(defun object-name-and-pkey-value (object)
  "=> CONS of name and value

If value is a list, return as a list, otherwise a single pkey"
  (etypecase object
    (ssconvert-type (list "ssconvert-type" (ssconvert-type-value object)))
    (t (let* ((name (string-downcase (class-name (class-of object))))
	 (pkeys (remove-if-not #'rofl::slot-definition-primary-key-p 
				(c2mop:class-slots (class-of object))))
	 (value (loop for pk in pkeys collect 
		     (handler-case (c2mop:slot-value-using-class 
				    (class-of object) object pk)
		       (error (c)
			 (when maxclaims::*debug-on-error*
			   (error c)))))))
    (cons name (if (rest value)
		   value
		   (first value)))))))

(defmacro <object-link ((&key object place tag-attributes
			      uri-attributes)
			&body body)
  
  `(destructuring-bind (name . value)
       (object-name-and-pkey-value ,object)
     (if value
	 (<:a :href (format nil "~A?~A=~S~{&~{~A=~A~}~}" 
			    ,place name value
			    (let ((go-back (or (hunchentoot:parameter "claim")
					       (hunchentoot:parameter "go-back-to-claim"))))
			      (if go-back
				(list* `("go-back-to-claim" ,go-back)
				       ,uri-attributes)
				,uri-attributes)))
	      :target "_top"
	      ,@tag-attributes
	      ,@body)
	 (progn 
	   (when maxclaims::*debug-on-error*
	     (error "no link for ~A ~A" ,object ,place))))))

(defmacro %link (object place att-list &body body)
  `(<object-link (:object ,object
		  :place  ,place
		  :tag-attributes ,att-list)
     ,@body))

(defmacro <view-link (object &body body)
  (destructuring-bind (object &rest tag-attribute-list)
      (if (listp object) object (list object))
    
    `(%link ,object "view" ,tag-attribute-list ,@body)))

(defmacro <create-link (object &body body)
  (destructuring-bind (object &rest tag-attribute-list)
      (if (listp object) object (list object))
    `(%link ,object "create" ,tag-attribute-list ,@body)))

(defmacro <delete-link (object &body body)
  (destructuring-bind (object &rest tag-attribute-list)
      (if (listp object) object (list object))
    `(%link ,object "delete" ,tag-attribute-list ,@body)))

(defmacro <edit-link (object &body body)
  
  `(progn 
	  (%link ,object "edit" nil ,@body)))

(defmacro <search-link (object &body body)
  `(%link ,object "search" nil ,@body))

(defmacro <previous-maxclaims-view-link (object &body body)
  `(let* ((name (string-downcase (class-name (class-of ,object))))
	  (pkeys (remove-if-not #'rofl::slot-definition-primary-key-p 
				(c2mop:class-slots (class-of ,object))))
	  (value (ignore-errors (c2mop:slot-value-using-class 
				 (class-of ,object) ,object (first pkeys)))))
     (if value
	 (<:a :href (format nil "/maxclaims/view?~A=~A" name value)
	       ,@body)
	 (progn ,@body))))

(defparameter *activate-function-html-tag-map*
  (macrolet ((tagfn (yaclml-tag &rest key-args)
	       `(lambda (object function &key ,@key-args)
		  (declare (ignore object))
		  (,yaclml-tag 
		   ,@(loop for arg in key-args
			:nconc (let ((name (if (listp arg) (first arg) arg)))
				 (list (intern (symbol-name name) :keyword)
				       name)))
		   (funcall function))))
	     (tags (&body tag-defs)
	       `(list ,@(loop :for tag-def 
			   :in tag-defs
			   :collect (destructuring-bind 
					  (tag . key-args) tag-def
					    `(cons ',tag 
						   (tagfn ,tag ,@(if key-args
								     key-args
								     (list '(style ""))))))))))
    (tags 
     (<:h1) (<:h2) (<:h3) (<:h4) (<:h5)
     (<:span class style)
     (<:blockquote class style)
     (<:p class style)
     (<:a (href "#"))
     (<:small)
     (<:pre (style "white-space: pre-wrap;       /* css-3 */
 white-space: -moz-pre-wrap;  /* Mozilla, since 1999 */
 white-space: -pre-wrap;      /* Opera 4-6 */
 white-space: -o-pre-wrap;    /* Opera 7 */
 word-wrap: break-word;       /* Internet Explorer 5.5+ */")))))

(defgeneric activate-lambda (name)
  (:method (name)
    (let ((tag (cdr (assoc name *activate-function-html-tag-map*
			   :test #'string=))))
      (or tag 
	  (lambda (object function)	      
	    (<:fieldset 
	     (<:legend (<:as-html name))
	     (<:as-html object)
	     (<:hr)
	     (<:as-html function))))))
  (:method 
      ((name (eql 'maxclaims/ecm-description:link-to-viewer)))
    (lambda (o f) 
      (handler-case
	  (etypecase o
	    (list (funcall f))
	    (t 
	     (<view-link o 
	       (funcall f))))
	(error (c)
	  (if maxclaims::*debug-on-error* 
	      (error c)
	      (<:as-html "Error for link-to-viewer: " o " " f)))))))

(defun activate (object fns fn)
  (if (not fns)
      (funcall fn)
      (let 
	  ()
	(destructuring-bind (name &rest args) 
	    (typecase (first fns)
	      (list (first fns))
	      (t (list (first fns))))
	  (apply  (activate-lambda name)
		  object (if (rest fns)
			(lambda () (activate object (rest fns) fn))
			fn)
		  args)))))


(defvar *standard-objects-displayed* nil)

(defun create-a-btn (&key 
		       (create "claim")
		       (access "risk")
		       (id "1")
		       (key "risk-id")
		       (current-tab NIL)
		       (attributes #+NIL'(("from" 1))))
  (<:a 
   :class "btn btn-success" 
   :target "_top"
   :href (apply #'concatenate 
	  'string 
	  `(,@(list  
	       "create?create[type]="
	       (string-downcase (princ-to-string create))
	       "&create[key]=" (string-downcase (princ-to-string key))
	       "&access[type]=" (string-downcase (princ-to-string access))
	       "&access[id]=" (princ-to-string id))
	      ,@ (when attributes
		   (loop for (k v) in attributes
			:nconc (list (concatenate
				      'string "&attribute["
				      k "]=" (princ-to-string v)))))
	      ,@ (when current-tab 
		   (list "&back[active-tab]="
			 current-tab)))
	  ) 
   (<:as-html "create new")))
  

(defvar *current-tab* nil)

(defgeneric display-as-table (object layers 
			      &rest args &key &allow-other-keys))

(defgeneric display (object layers  &rest args &key &allow-other-keys)
  (:method (object layers &rest args)
    (declare (ignore args))
    (<:as-html (princ-to-string object)))
  (:method ((object list) layers 
	    &rest args &key as-table 
		      offset 
		      (limit 25)
		      &allow-other-keys)
    (if as-table
	(apply #'display-as-table object layers args)
	(<:ul 
	 :class "inline"
	 ;;(break "~A ~A ~A" object layers a)
	 (loop :for (o . rest) :on object
		     :do 
		     (<:li 
		      :style "padding:0.5em; margin-bottom:3px;border:1px solid #EFEFEF ;background-color: white"
		      :class "attribute-li"	     
		      (apply #'display o layers :label nil args)))))
        
    (when offset
      (let ((offset (typecase offset
		      (symbol (funcall offset))
		      (T offset))))
	(when (> offset 0)
	  (<:a :class "btn btn-info"
	       :href (format nil "?~{~A=~A~}&tab[active]=~A&offset[~A]=~A" 
			     *view-name-and-pkey-value* 
			     *current-tab*
			     *current-tab*
			     (- offset limit )) 
	       (<:as-html "< Previous ("(- offset limit ))")" ))
	(if (= (length object) limit)
	    (<:a :class "btn btn-info"
		 :href (format nil "?~{~A=~A~}&tab[active]=~A&offset[~A]=~A" 
			       *view-name-and-pkey-value* 
			       *current-tab*
			       *current-tab*
			       (+ limit offset)) 
		 (<:as-html "Next ("(+ limit offset))") >" )))))


  (:method ((object standard-object) layers 
	    &rest args
	    &key 
	      as-table-row
	      (attributes NIL)
	      )
    #+foo (typecase object
	    (maxclaims/ecm-description::risk-risk-detail 
	     (break "~A: ~A ~A" object layers args)))
    (let ((*standard-objects-displayed* (cons object *standard-objects-displayed*))
	  (as (apply #'maxclaims/ecm-description::ecm-attributes object layers args))
	  (edit-as (ignore-errors (apply #'maxclaims/ecm-description::ecm-attributes object :edit args))))
      
      ;;   (break "atttributes : ~A" object as)
      (if as
	  (dolist (a as)
	    (let ((label (maxclaims/ecm-description::attribute-label a))
		  (value (maxclaims/ecm-description::attribute-value a))
		  (alayers (maxclaims/ecm-description::attribute-layers a))
		  (inline-edit (getf (rest a) :inline-edit)))
	      
	      (when (or as-table-row 
			(and value 
			     (not (equalp "" value))))
		(let ((att 
		       (lambda ()
			 (macrolet ((inline-edit (&body body)
				      `(if (and inline-edit 
						(user-can-edit-p object)
						(or #+(or)(user-is-adjuster-p maxclaims::$app-user)
						    (maxclaims::app-user.admin 
						     maxclaims::$app-user)))
					   (<:div :data-inline-edit 
						  (max-ecm/json:write-json-to-string
						   (destructuring-bind (type . value)
						       (object-name-and-pkey-value object)
						     (max-ecm/json:jso
						    "type" type
						    "id" value
						    "attribute" 
						    (string-downcase
						     (maxclaims/ecm-description:attribute-name a))
						    "label" label)))
						  :style "width:100%; 
                                                          height: 100%; 
                                                          min-height:1em;"
;;;                                                          border:1px solid red
						  ,@body)
					   (progn ,@body))))
			   (inline-edit 
			    (if (or (eq T value)
				    (eq NIL value))
				(<:as-html (if value "Yes" "No"))       
				(apply #'display 
				       value alayers 
				       :attributes (getf (rest a) :attributes)
				       :label (and (not as-table-row) label)
				       :activate (getf (rest a) :activate)
				       (rest a))))))))
		  (<:as-html " ")
		
		  ;;		  (break "activate for ~A? ~A~%~A " object (getf  (rest a) :activate) (rest a))
		  (funcall (if as-table-row
			       (lambda ()
				 (<:as-is 
				  (yasexml:<> `(td ,@(getf (rest a) :td))
				    (yasexml:<> (:unescaped (yaclml:with-yaclml-output-to-string
				  (funcall att)))))))
			       att)))
		
		)))
	  (progn			;(break "~A" object)
	    (<:as-html "no " layers " found for " (type-of object)))))))

(defmethod display :around (object layer 
			    &key activate label active
			      &allow-other-keys)
  ;; (when label (break "~A~%~A" (list object layer label) args))
  (flet ((doit () (activate object activate 
		    (lambda () 
		      (when label 
			(<:span 
			 :class "muted attribute-label"
			 (<:as-html label " "  )))
		      (call-next-method)))))
    (when (not (and (eq :when active)
		    (null object)))
      (doit))))
	   

(defmethod display ((object standard-object) 
		    (layer (eql :inline-block))
		    &rest args)
  
  (let ((br 
	 (apply #'maxclaims/ecm-description::ecm-attributes object layer args)))
    (if br
      (<:ul 
	:class "inline" 
	(dolist (ar br)
	  (let ((label 
		 (maxclaims/ecm-description::attribute-label ar))
		(value (apply #'maxclaims/ecm-description::object-attribute-value
			object (first ar) (rest ar))))
	    (when value 
	      (<:li 
	       :class "attribute"
	       (apply 
		#'display value
		:inline :label label (rest ar)))))))
      (<:as-html "No atts for" object " " args))))
