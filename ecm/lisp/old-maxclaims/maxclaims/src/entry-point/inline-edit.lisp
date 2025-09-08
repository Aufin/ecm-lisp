(defpackage :maxclaims/entry-point/inline-edit
  (:use :cl 	:maxclaims/web-display/display)
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
		#:back-to-create/edit)

  (:import-from :maxclaims
		#:with-udb
		#:call-with-app-user
		#:find-object)
  (:import-from :max-ecm/json 
		#:getjso
		#:jso
		#:write-json-to-string
		#:read-json-from-string)
  (:import-from :max-ecm/client)
  (:export))
  

(in-package :maxclaims/entry-point/inline-edit)

(defmacro with-app-user (() &body body)
  `(call-with-app-user 
    (get-app-user) 
    (lambda () 
      (with-udb
	,@body))))

(defun name->json-name (string)
  (substitute #\_ #\- (string-downcase string)))

(defun jso-to-label (jso)
  (with-output-to-string (s)
    (max-ecm/json:mapjso 
     (lambda (k v) 
       (if (not (member k '("id" "type") :test 'string=))
	   (unless (or (not v)
		       (eql v :null))
	     (format s "~A " v))))
     jso)))

(defun inline-search (search name type)
  (let* ((result (max-ecm/client:jso-rpc 
		 "search" 
		 (jso "type" type
		      "q" search)))
	 (page (getjso "result_page" result))
	 (fname (format nil "future[~A]" name))
	 (sname (format nil "search[~A]" name)))
    (if page
	(progn 
	  (<:script 
	   (<:as-is '| $('[name="| sname '|"]').val("") |))
	  (<:select :size 5 
		  :name fname 
	  (loop for o in page 
	     :for label = (jso-to-label o)
	       :do (<:option :label label :value (getjso "id" o)
			     (<:as-html label))))
	  (<:as-is '#:|
    
    <button type="submit" class="btn btn-success">Save</button>
    <button class="btn btn-warning" aria-hidden="true" data-dismiss="modal">Cancel</button>
    |)
					      
	  ;;(<:as-html (write-json-to-string page))
	 )
	(<:as-html (write-json-to-string result
					 )))))
	
  

(define-easy-handler (inline-edit-handler :uri "/ecm/inline-edit")
    (json)
  (with-app-user ()
    (let* ((jso (max-ecm/json:read-json-from-string json))
	   (type (intern (symbol-name (read-from-string 
				       (concatenate 'string "#:" (getjso "type" jso))))
			 :maxclaims/ecm-description))
	   (pkey (getjso "id" jso))
	   (object (find-object type pkey))
	   (attribute-name (getjso "attribute" jso))
	   (attribute (assoc attribute-name (maxclaims/ecm-description:ecm-attributes 
					     object :edit)
			     :test #'string-equal))
	   (request-method 
	    (hunchentoot:request-method*)))
      (if (string= request-method "GET")
	  (<:div :class "modal-body"
		 (<:div 
		  :class "modal-header"
		  (<:as-is '#:|<button type="button" data-dismiss="modal"  class="close inline-close"  aria-hidden="true">×</button>|)
		  (display object :heading))
  
		 (<:form 
		  :class "inline-edit-form"
		  :method "POST"		  		
		  (<:div 
		   :class "container"
		   ;;(<:as-html request-method)
		   (<:input :type "hidden" :name "json" :value json)
		   (maxclaims/web-display/edit:edit-attribute-block
		    object attribute (list attribute) nil nil t :edit)
		   (let ((allow-null? (getf (rest attribute) :allow-null?)))
		     (when allow-null?
		     		  
		       (<:div 
			:class "row-fluid"
			(<:div :class "span2")
			(<:div 
			 :class "span10"
			 (<:label 
			  (<:as-is '| Or: <a data-toggle="tooltip" title="|)
			  (<:as-html allow-null?)
			  (<:as-is '|"> Mark as NULL?  </a>|)
			  (<:input :type "checkbox"
				   :name (concatenate
					  'string "null["(string (first attribute))"]")))))))
		   (<:div :class "inline-edit-output"))
		  (<:as-is '#:| <div class="modal-footer">
    
    <button type="submit" class="btn btn-success">Save changes</button>
    <button class="btn btn-warning" aria-hidden="true" data-dismiss="modal">Close</button>

  </div>|))
		 (<:script
		  (<:as-is '#:| $('[data-toggle="tooltip"]').tooltip()
       $('[class="inline-edit-form"]')
         .submit(function (event) 
                  {event.preventDefault();
                   $.post("/ecm/inline-edit", 
                          $( this ).serialize(),
                           (function (data) {
                                     $('[class="inline-edit-output"]').empty().append(data);
                                    }))}); 
|)))
;;;ELSE NOT (string= request-method "GET")
	  (let ((present (loop for (n v) in 
			      (http-parameters-changed-alist 
			       "past" "future")
			    :collect (list (name->json-name n)
					   v)))
		
		(search (first (loop for v in (http-parameters-as-alist "search")
				  :collect (second v))))
		(null (http-parameters-as-alist "null"))
		#+(or)(search-type (http-parameters-as-alist "type"))
		(type (getjso "type" jso))
		(attribute-type (getf (cdr attribute) :type)))
	    (cond 
	      ;; Searching for something
	      ((and search (not (string= "" search))
			#+(or)(not present))
	       (inline-search search (first attribute) (string-downcase attribute-type)))
		   
	      ;; Some values have been set, update them.
	      ((or present null)
	       (let* ((update (max-ecm/json:jso 
			       "type" type
			       "id" (getjso "id" jso)))
		      (result (loop :for (k v) :in (or present null)
				  :do (setf (getjso (name->json-name k) update)
					    (if (and (not present) null)
						:null 
						v))
				  :finally (return 
					     (max-ecm/client:jso-rpc 
					      "update" update 
					      :http-user
					      (list (maxclaims::app-user.username 
						     maxclaims::$app-user)
						    (maxclaims::app-user.password
						     maxclaims::$app-user))))))
		       (response (getjso "response" result))
		       (jsoname (name->json-name attribute-name))
		       (value (when response (getjso  jsoname response))))
		  (if response
		      (let ((js-value (etypecase value 
					(string value)
					(integer value)
					(symbol (if (eql value :null)
						    ""
						    value))
					(jso (jso-to-label value)))))
		      (<:script	
		       
		       (<:as-is 
		    
			'#:| 
$("[data-inline-edit]").filter(function() { ie = $(this).data("inline-edit"); console.log( "| jsoname'#:|")
                                          return (ie.id === | (getjso "id" response) 
					     '#:| && ie.attribute === "|
			attribute-name '#:|")}).empty().append("| js-value '#:|");
$('#myModal').modal('toggle');|)))		      
		      (<:as-html (max-ecm/json:write-json-to-string result)))))
		  (t 
		   (<:code (<:as-html 
			    "Nothing to Save : " 
			    (http-parameters-as-alist))))))))))








  
