(defpackage :maxclaims/entry-point/toplevel
  (:use :cl)  
  (:import-from #:maxclaims/hunchentoot
		#:query-string*)
  (:import-from :maxclaims	
		#:with-udb
		;; Object types
		#:claim
		#:policy
		#:person
		#:contract
		;; Filters
		#:filter-objects)
  (:import-from :rofl 
		#:slot-definition-primary-key-p
		#:select-objects)
  (:export 
   #:http-query-string
   #:map-http-parameters
   #:http-parameter-value
   #:http-parameters-as-alist
   #:http-parameters-changed-alist
   #:select-objects/key-value
   #:select-object-from-request
   #:object-typename-and-id-from-request))

(in-package :maxclaims/entry-point/toplevel)

(defun http-query-string (&optional http-request)
  (apply #'query-string* 
	 (when http-request (list http-request))))

(defun map-http-parameters (|function (name value)|)
  (loop :for (name . value) 
     :in (append (hunchentoot:get-parameters*)
		 (hunchentoot:post-parameters*))
     :do  (funcall |function (name value)| name value)))

(defun http-parameter-value (name 
			     &optional 
			       (parameter #'hunchentoot:parameter))
  (funcall parameter name))

(defun http-parameters-as-alist (&optional alist-name)
  (let (alist)
    (map-http-parameters
     (lambda (n v)
       (when (stringp v)
	 (if alist-name
	     (when (string= 
		    alist-name
		    (subseq n 0 (min (length n) 
				     (length alist-name))))
	      #+(or) (break "'~A': '~A' ~A" alist-name 
		      (subseq n (1+ (length alist-name))
			      (1- (length n))) n)
	       
		  (push (list (subseq n (1+ (length alist-name))
				      (1- (length n)))
			      v)
			alist))			       
		(push (list n v) alist)))))
    (nreverse alist)))

(defun http-parameters-changed-alist (past-name future-name)
  (let ((past (http-parameters-as-alist past-name))
	(future (http-parameters-as-alist future-name)))
    ;;(break "~A ~A" past future)
    (loop for (name value) in future
       :unless (string= value (second (assoc name past
					   :test #'string=)))
	 :collect (list name value))))
  
(defun select-objects/key-value (type key value
				 &key (filter #'filter-objects)
				   (where nil))
  (with-udb 
    (let* ((objects (select-objects 
		     type
		     :where `(:and (:= ,key ,value)
				  ,(or where t)))))
      (when objects (funcall filter objects)))))

(defun object-typename-and-id-from-request ()
  (loop :for (name . value) 
     :in (hunchentoot:get-parameters*)
     :do  (unless (find #\[ name)
	    (return (list name value)))))


(defun select-object-from-request ()
  ;; right now there is only one "id" and therefore only one primary key is supported
  (with-udb
    (multiple-value-bind (typename id)
	(loop :for (name . value) 
	   :in (hunchentoot:get-parameters*)
	   :do  (unless (find #\[ name)
		  (return (values name value))))
      (let* ((type (intern (substitute #\- #\_ (string-upcase typename)) 
			   (cond ((string= typename "app-user-message")
				  :maxclaims/data-entity/app-user-message)
				 (t :maxclaims))))
	     (class (find-class type))
	     (pkeys (mapcar #'c2mop:slot-definition-name 
			    (remove-if-not #'slot-definition-primary-key-p 
					   (c2mop:class-slots class))))
	     (id-value (let ((*read-eval* nil))
			 (read-from-string id)))
	     (id-list (if (listp id-value)
			  id-value 
			  (list id-value)))
	     (objects (select-objects 
		       type
		       :where `(:and ,@(loop :for name :in pkeys
					  :for value :in id-list
					  :collect  `(:= ,name ,value))))))
	(when objects 
	  (typecase (first objects)
	    (maxclaims/data-entity/app-user-message:app-user-message 
	     (first objects))
	    (t (first (filter-objects objects)))))))))
