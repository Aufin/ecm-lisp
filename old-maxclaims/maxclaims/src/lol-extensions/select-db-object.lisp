(in-package :maxclaims)

;; * DB-OBJECT :input types.
(deftype select-db-object () '(or list string standard-db-access-object))

(defclass select-db-object-attribute-editor (attribute-editor)
  ((db-type :accessor db-type :initarg :db-type)
   (size :accessor select-size :initarg :size :initform 2)
   (search-value-table :accessor search-value-table
		       :initform (make-hash-table :synchronized t
						  :weakness :key))))

(define-layered-function display-select-db-object-value
    (value editor writer attribute selected-value reset))

(define-layered-function display-select-db-object-value-items
    (value editor writer attribute selected-value)
  (:documentation "Display VALUE a (cons SEARCH-ITEM . SEARCH-RESULTS)"))

(defmethod edit-the-value (edited-object-class-name attribute-name)
  t)

(defmethod edit-the-value ((e standard-db-access-object) 
			   (a slot-definition-attribute))
  (edit-the-value 
   (class-name (class-of e))
   (lol::attribute-name a)))

(defun bound-slot-names (object)
  (let ((class (class-of object)))
    (loop :for slotd in (closer-mop:class-slots class) 
       :when (closer-mop:slot-boundp-using-class class object slotd)
       :collect (closer-mop:slot-definition-name slotd))))

(defaction find-db-object-using-attribute-editor-action (attribute editor object)
  t)

(defvar *containing-attribute* nil)

(defstruct partial-result
  attribute
  attribute-object
  result-object)

(defmacro %let-id-kp ((id kp) &body body)
  `(let* ((,id (arnesi:random-string))
	  (,kp (format nil "if(event && event.keyCode == 13)
   {
     document.getElementById('~A').click();
   }" ,id)))
     ,@body))

(defvar *string-select-where-search* nil)

(defmethod select-using-db-object (new)
  (let* ((*string-select-where-search* 
	   (lambda (a b) `(:= ,a ,b)))
	 (res1 (select-using-object new))
	 (*string-select-where-search* 
	   (lambda (a b) `(:ilike ,a ,b)))
	 (res2 (select-using-object new)))
    (dolist (r res1)
      (setf res2 (remove (object-id r) res2
			 :key #'object-id
			 :test #'equal)))
    (append res1 res2)))



(defun render-search-or-create (editor writer attribute
				&optional (new (make-instance (db-type editor))
					       new-p))
  #+nil(break "rsc ~A/~A (~A)" attribute new new-p)
  (let* ((*containing-attribute* (make-partial-result
				  :attribute attribute
				  :attribute-object (attribute-object attribute)
				  :result-object new))
	 (bound-slots (bound-slot-names new))
	 (callback (register-callback 
		    (lambda (val)
		      (declare (ignore val))
		      (when (and (bound-slot-names new)
				 (not (equalp bound-slots (bound-slot-names new))))
			;(break "writing ~A" new)
			(funcall writer (cons new (select-using-db-object new))))))))
    (%let-id-kp (id kp)
      (<:div :onkeypress kp
       (cond ((lol::attribute-editor-attributes editor)
	      (read-object new :attributes (lol::attribute-editor-attributes editor)))
	     ((ignore-errors (lol::attribute-active-attributes attribute))
	      (read-object new :attributes (lol::attribute-active-attributes attribute)))
	     (t 
	      (read-object new))))
     #+nil(break "rsc after read-object ~A" attribute)
     (<:input :type "hidden" :name callback :value nil)
     (<ucw:submit :id id
		  :action (find-db-object-using-attribute-editor-action
			   attribute editor new)
		  "select"))))

 (define-layered-method display-select-db-object-value ((value partial-result) editor writer attribute selected-value reset)
			(render-search-or-create editor writer attribute
						 (partial-result-result-object value)))

 (define-layered-method display-select-db-object-value (value editor writer attribute selected-value reset)
   (if (or (lol::unbound-slot-value-p value) (null value))
       (render-search-or-create editor writer attribute)
       (<:as-html "Don't understand " value)))

 (define-layered-method display-select-db-object-value ((value null) editor writer attribute selected-value reset)
   (render-search-or-create editor writer attribute))

 (define-layered-method display-select-db-object-value ((value standard-db-access-object) editor writer attribute selected-value reset)

   (with-inactive-descriptions (editable)
     (if (primary-key-boundp value)
	 (with-active-descriptions (link-to-viewer)
	   (display-inline-attribute attribute value ))
	 (with-active-descriptions (link-to-editor)
	   (display-inline-attribute attribute value ))))
   (<:as-html "  ")
   (when (edit-the-value 
	  (first *edited-object*)
	  attribute)		     	 
    (<ucw:a 
     :action (funcall reset)
     (<:as-html "(reset)"))))

(defstruct %select-db-search-results 
  list)

(defun %d-select-db (value editor writer attribute selected-value reset
		     &key search)
  (%let-id-kp (id kp)
    (<:div 
     :onkeypress kp
     (with-inactive-descriptions (editable)
       (<ucw:select 
	:size (min (length value) (select-size editor))
	:writer writer
	(display-select-db-object-value-items 
	 value editor writer attribute selected-value))))
    (<ucw:submit 
     :id id
     :action (select-db-object-using-editor attribute editor selected-value)
     "select")
    (<ucw:submit :action (funcall reset)
		 "reset")
    (when search
      (<ucw:submit :action (funcall writer (make-%select-db-search-results 
					    :list (cons (car value)
							(select-using-object 
							 (car value)))))
		   "search"))))


(define-layered-method display-select-db-object-value ((value list) editor writer attribute selected-value reset)
   #+nil(break "list?")
   (%d-select-db value editor writer attribute selected-value reset :search t))

(define-layered-method display-select-db-object-value ((value %select-db-search-results) editor writer attribute selected-value reset)
   #+nil(break "list?")
   (%d-select-db 
    (%select-db-search-results-list value)
    editor writer attribute selected-value reset ))

(define-layered-method display-select-db-object-value-items ((value list)
							     editor
							     writer
							     attribute
							     selected-value)
  #+nil(break "items?")
  (when (= (select-size editor) 1)
    (<ucw:option :value lol::+unbound-slot+
		 (<:as-html "---")))
  (let ((selected (funcall selected-value)))
    (arnesi:dolist* (val (cdr value))
      #+nil(break "item ~A")
      (<ucw:option :value val
		   :selected (ignore-errors (db= val selected))
		   (<:as-html "Select: ")
		   (<:as-html  (let ((*current-component* nil))
				 (with-inactive-descriptions (html-description)
				   (display-inline-attribute attribute val))))))))

;;; todo: this is pretty ugly
(defmethod display-html-attribute-editor (attribute
					  (editor select-db-object-attribute-editor))
  (let* ((writer (let ((%w (make-attribute-value-writer attribute))
		       (containing-attribute *containing-attribute*))
		   ;; todo: call (setf (attribute-value ...) ...)
		   ;; directly (but not that `make-attribute-writer'
		   ;; contains code to ignore values like "" so the
		   ;; editor unbound-value code needs to be made
		   ;; active)
		  (dlambda (val)
		    #+nil(break "w ~A ~A ~A" containing-attribute
			   attribute
			   val)
		    (cond ((typep val (db-type editor))
			   (remhash (attribute-object attribute)
				    (search-value-table editor))
			   (funcall %w val))
			  (t
			   (setf (gethash (attribute-object attribute)
					  (search-value-table editor)) val)
			   (when-bind ca containing-attribute
			     (setf (gethash (partial-result-attribute-object ca)
					    (search-value-table
					     (attribute-editor
					      (partial-result-attribute ca))))
				   (prog1 ca
				     (unless (partial-result-result-object ca)
				       (setf (partial-result-result-object ca)
					     (attribute-object attribute)))))))))))
	 (reset (dlambda ()
		  (remhash (attribute-object attribute)
			   (search-value-table editor))
		  (lol::attribute-slot-makunbound attribute)))
	 (value (if (lol::unbound-slot-value-p
		     (gethash (attribute-object attribute)
			      (search-value-table editor)
			      lol::+unbound-slot+))
		    (attribute-value attribute)
		    (gethash (attribute-object attribute)
			     (search-value-table editor))))
	 (selected-value (dlambda ()
			   (lol::attribute-value attribute))))
    #+nil(break "h-a-e: ~A/~A/~A/~A/~A/~A" editor attribute (attribute-object attribute) (funcall selected-value) value *containing-attribute*)
    (display-select-db-object-value value editor writer attribute
				    selected-value reset)))

(defaction select-db-object-using-editor (attribute editor select-value)
  (let ((value (funcall select-value)))
    (if (and (typep value 'standard-db-access-object) 
	     (primary-key-boundp value))
	t
	(if (listp value)
	    nil
	    (create-object value)))))

;;; Writeable select
(define-description writeable-select ()
  ())

(define-layered-method display-select-db-object-value-items
  :in #. (defining-description 'writeable-select)
  :before
  ((value list) editor writer attribute selected-value)
  (<ucw:option  :value (first value)
		:selected (when (endp (rest value)) t)
		(<:as-html "Create New ")
		(<:as-html  
		 (let ((*current-component* nil))
		   (with-inactive-descriptions (html-description)
		     (display-inline-attribute attribute (first value)))))))

;;; Prepopulated db select
(defun prepopulated-select-default (editor)
  (select-objects (db-type editor)))

(define-description prepopulated-select ()
  ((select-fn :keyword :select-function
	      :value 'prepopulated-select-default)))

(define-layered-method display-select-db-object-value
    :in #.(defining-description 'prepopulated-select)
    :around
    (value editor writer attribute selected-value reset)
  (if (and (layer-active-p (defining-description 'editable))
	   (lol::unbound-slot-value-p value))
      (let ((av (attribute-value (find-attribute 'prepopulated-select
							 'select-fn))))
	(with-inactive-descriptions (prepopulated-select)
	  (display-select-db-object-value
	   (cons (make-instance (db-type editor))
		 (funcall av
			  editor))
	   editor writer attribute selected-value reset)))
      (call-next-layered-method)))
