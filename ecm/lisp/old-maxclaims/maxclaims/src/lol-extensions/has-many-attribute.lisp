(in-package #:maxclaims)

;;; * Has-Many Attribute for `db-access-object's

(defvar *default-has-many-page-size* 20
  "Default page size for a has-many attribute views")

(define-layered-class has-many-attribute (slot-definition-attribute)
  ((sort :initarg :sort :initform '(< :key object-id))
   ;; fixme: kluge alert, should rofl instead provide more general
   ;; :references slots that support filtering?
   (filter :initarg :filter)
   (paged :initarg :paged :initform nil :reader page-size
	  :documentation "When T or a positive integer, page the
results view. When T pages are of length
*DEFAULT-HAS-MANY-PAGE-SIZE*; when an integer pages are that
size.")))

(defmethod page-size :around ((attribute has-many-attribute))
  (let ((slot-value (call-next-method)))
    (cond ((eq slot-value nil) nil)
	  ((eq slot-value t) *default-has-many-page-size*)
	  ((and (integerp slot-value) (plusp slot-value)) slot-value))))

(defaction create-new-has-many (object attribute)
  (with-described-object (object (lol::attribute-description attribute))
    (let* ((slotd (find (attribute-slot-name attribute) 
			(lol::class-slots (class-of object))
			:key (function rofl::slot-definition-name)))
	   (related-object (rofl::make-object 
			    (rofl::slot-definition-foreign-relation slotd)))
	   (related-slotd (find (or (rofl::slot-definition-foreign-join-spec slotd) 
				    (rofl::class-id-column-name (class-of object)))
				(lol::class-slots (class-of related-object))
				:key (function rofl::slot-definition-column-name))))

      
      (setf (rofl::slot-value-using-class (class-of related-object) related-object related-slotd)
			
	    (slot-value object (or (rofl::slot-definition-column-name slotd)
 
				   (rofl::class-id-column-name (class-of object)))))
      (slot-makunbound object (rofl::slot-definition-name slotd))
      (create-object  related-object))))

(defcomponent has-many-paged-list (paged-list)
  ((attribute :initarg :attribute :reader has-many-paged-list.attribute)
   (object :initarg :object :reader has-many-paged-list.object)
   (args :initarg :args :reader has-many-paged-list.args)))

(defmethod render ((list has-many-paged-list))
  (let ((rows (paged-list.current-page list))
	(object (has-many-paged-list.object list))
	(attribute (has-many-paged-list.attribute list))
	(args (has-many-paged-list.args list)))
    (let ((lol:*display* list))
      (declare (special *display*))
      (<:table
       (apply #'lol::funcall-with-described-object
	      (lambda ()
		(let ((attributes (with-active-descriptions (inline)
				    (attributes *description*))))
		  (<:tr (<:th
			 (unless (user-read-only-p $app-user)
			   (if ucw::*in-form*  
			       (<ucw:submit :action (create-new-has-many object attribute) "Create New") 
			       (<ucw:form :action (create-new-has-many object attribute)
					  (<:submit :value "Create New")))))		    

			(when rows 
			  (dolist (a attributes)
			    (<:th (display-attribute-label a)))))  
       
		  (when rows
		    (flet ((display-rows (rows)
			     (dolist (r rows)
			       (with-described-object (r nil)
				 (<:tr (let ((object (attribute-object (first  attributes))))
					 (<:td 
					  (<ucw:a :action (view-object object)
						  (<:as-html "View"))
					  (when (user-can-edit-p object)
					    (<:ah " ")
					    (<ucw:a :action (edit-object object)
						    "Edit")))) 
				       (dolist (a attributes)
					 (with-attribute-context (a)
					   (<:td (when (lol:attribute-active-p a)
						   (with-active-descriptions (inline)
						     (display-attribute-value a)))))))))))
		      (display-rows rows)))))
	      (when (consp rows) (first rows))
	      nil
	      (when rows args)))
      (<ucw:form :action (refresh-component list)
	(<:p (<:format "Page ~D of ~D"
		       (1+ (paged-list.offset list))
		       (paged-list.page-count list)))
	(<ucw:submit :action (scroll-forward list) "Next")
	(<ucw:submit :action (scroll-backward list) "Previous")
	(<:br)
	(<ucw:submit :action (answer) "Return")))))


(defmethod display-has-many-attribute-p (attribute a)
  (and (not (lol::unbound-slot-value-p
	     (attribute-value a)))
       (lol:attribute-active-p a)))
  
(defmethod display-has-many-table-rows (attribute object rows args)
  (apply 
   #'lol::funcall-with-described-object
   (lambda ()
     (let ((attributes (with-active-descriptions (inline)
			 (attributes *description*))))
       
       (<:thead 
	(<:tr (<:th (unless (user-read-only-p $app-user)
		      (if ucw::*in-form*  
			  (<ucw:submit :action (create-new-has-many object attribute) "Create New") 
			  (<ucw:form :action (create-new-has-many object attribute)
				     (<:submit :value "Create New")))))		    

	      (when rows 
		(dolist (a attributes)
		  (<:th (display-attribute-label a))))))  
       
       (when rows
	 (flet ((display-rows (rows)
		  (dolist (r rows)
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
					  (display-attribute-value a)))))))))))
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

  
(define-layered-method 
    lol::display-html-attribute-value (object (attribute has-many-attribute))
  (<:td 
   (let* ((rows (let ((value (attribute-value attribute)))
		  (unless (lol::unbound-slot-value-p value)
		    (apply #'sort (funcall (if (slot-boundp attribute 'filter)
					       (curry #'remove-if-not
						      (slot-value attribute 'filter))
					       #'copy-list)
					   (attribute-value attribute))
			   (slot-value attribute 'sort)))))
	  (active-attributes (ignore-errors (lol::attribute-active-attributes attribute)))
	  (args (when (and active-attributes (listp rows)) (list :attributes active-attributes))))
     (<:table     
      (display-has-many-table-rows attribute object rows args)))))
