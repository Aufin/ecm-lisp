(in-package :rofl)

(defclass db-access-slot-definition ()
  ((column-name  :initform nil
                 :initarg :db-name
                 :initarg :column
                 :accessor slot-definition-column-name
               :documentation
              "If non-NIL, contains the name of the column this slot is representing.")
   (primary-key :initform nil
                :initarg :primary-key
                :accessor slot-definition-primary-key-p)
   (transient  :initform nil :initarg :transient :accessor slot-definition-transient-p
               :documentation
              "If non-NIL, this slot should be treated as transient and                                                      
ignored in all database related operations.")

   (foreign-type
    :initform nil
    :initarg :foreign-type
    :initarg :references
    :accessor slot-definition-foreign-type)
   (foreign-relation
    :initform nil
    :initarg :referenced-from
    :initarg :referenced-by
    :accessor slot-definition-foreign-relation)
   (foreign-join-spec
    :initform nil
    :initarg :on
    :initarg :using
    :accessor slot-definition-foreign-join-spec)
   (buffer-size
    :initform nil
    :initarg buffer
    :accessor slot-definition-buffer-size)))


(defclass db-access-class (standard-class)
  ((table-name :initarg :table-name 
	       :initarg :table 
	       :initform nil 
	       :accessor class-table-name))
  (:documentation "Metaclass for simple o/r."))

(defmethod validate-superclass
           ((class db-access-class)
            (superclass standard-class))
  t)


(defclass db-access-direct-slot-definition (standard-direct-slot-definition
					    db-access-slot-definition)
  ())

(defmethod direct-slot-definition-class
           ((class db-access-class) &key &allow-other-keys)
  (find-class 'db-access-direct-slot-definition))

(defclass db-access-effective-slot-definition 
    (standard-effective-slot-definition
     db-access-slot-definition)
  ())

(defmethod effective-slot-definition-class
           ((class db-access-class) &key &allow-other-keys)
  (find-class 'db-access-effective-slot-definition))

(defmethod compute-effective-slot-definition
           ((class db-access-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((slotd (call-next-method)))
    (setf (slot-definition-primary-key-p slotd) 
	  (some #'slot-definition-primary-key-p direct-slot-definitions)

	  (slot-definition-transient-p slotd) 
	  (every #'slot-definition-transient-p direct-slot-definitions)
	  (slot-definition-foreign-type slotd) 
	  (slot-definition-foreign-type (car direct-slot-definitions))
	  (slot-definition-foreign-relation slotd) 
	  (slot-definition-foreign-relation (car direct-slot-definitions))
	  (slot-definition-foreign-join-spec slotd) 
	  (slot-definition-foreign-join-spec (car direct-slot-definitions))
	  (slot-definition-type slotd) (slot-definition-type (car direct-slot-definitions)))

    (unless (slot-definition-foreign-relation slotd)
      (setf  (slot-definition-column-name slotd)
	     (or (let ((slot (find-if #'slot-definition-column-name direct-slot-definitions)))
		   (when slot
		     (slot-definition-column-name slot)))
		 name)))
    slotd))

(defun class-id-slot-definition (class)
  (find-if #'slot-definition-primary-key-p 
	   (class-slots class)))

(defmethod class-table-name :around (class)
  (or (first (call-next-method)) 
      (class-name class)))



(defmethod class-db-slots (class)
  (remove-if #'slot-definition-transient-p
	     (class-slots class)))

(defclass standard-db-access-class (db-access-class)
  ())

(defstruct (foreign-modification
	     (:conc-name fm-))
  operation
  instance
  slotd)

(defgeneric find-foreign-relations (class object slotd))

(defmethod find-foreign-relations ((class db-access-class) object slotd)
  ;; this assumes tha foreign-relations-makunbound does not reset
  ;; non-persistent instances
  (when (slot-boundp object (class-id-column-name class))
    (select-objects (slot-definition-foreign-relation slotd)
		    :where `(:= ,(or (slot-definition-foreign-join-spec slotd) 
				     (class-id-column-name class))
				,(slot-value object (class-id-column-name class))))))

(defun real-slot-boundp-using-class (class object slotd)
  (let ((real-slot-boundp-using-class t))
    (declare (special real-slot-boundp-using-class))
    (slot-boundp-using-class class object slotd)))

(defmethod slot-boundp-using-class :around 
    ((class standard-db-access-class) object slotd)
  (declare (special real-slot-boundp-using-class))
  (if (boundp 'real-slot-boundp-using-class)
      (call-next-method)
      (let ((bound? (call-next-method)))
	(if(and (not bound?) (slot-definition-foreign-relation slotd))
	   (when *database*
	     (let ((relations (find-foreign-relations class object slotd)))
	       (when relations  
		 (setf (slot-value-using-class class object slotd) relations
		       bound? t))))
	   bound?))))

(defmethod slot-value-using-class :around 
    ((class standard-db-access-class) object slotd)
  (cond ((slot-definition-foreign-relation slotd)
	 (if (slot-boundp-using-class class object slotd)
	     (call-next-method)
	     (setf (slot-value-using-class class object slotd) 
		   (find-foreign-relations class object slotd))))
	((slot-definition-foreign-type slotd)
	 (call-next-method))
	(t (call-next-method))))


(defun set-foreign-keys-from-foreign-type-slotd (value class object slotd)
  (let ((slots (remove-if-not 
		(lambda (slot)
		  (and (not (slot-definition-foreign-type slot))
		       (eq (slot-definition-column-name slot)
			   (slot-definition-column-name slotd))))
		(class-slots class))))
	(dolist (slot slots)
	  (setf (slot-value-using-class class object slot)
		(object-id value)))))

(defmethod (setf slot-value-using-class) :after 
    (value (class standard-db-access-class) object slotd)
 )

(defvar *previously-set-slots* '())

(defmethod (setf slot-value-using-class) :around 
    (value (class standard-db-access-class) object slotd)

  (let ((*previously-set-slots* (if *previously-set-slots* 
				    *previously-set-slots*
				    (make-hash-table :test #'eq))))
    (unless (member slotd (gethash  object *previously-set-slots*))
      (push slotd (gethash  object *previously-set-slots*))
      (call-next-method)
      (let ((foreign-object-slots 
	     (remove-if-not 
	      (lambda (slot)
		(and (slot-definition-foreign-type slot)
		     (eq (slot-definition-column-name slot)
			 (slot-definition-column-name slotd))
		     (not (eq slotd slot)))) 
	      (class-slots class))))
	(cond ((and value 
		    (typep value 'standard-db-access-object)
		    (slot-definition-foreign-type slotd)
		    (primary-key-boundp value))

	       (set-foreign-keys-from-foreign-type-slotd value class object slotd))
	      ((and value (slot-definition-foreign-type slotd)
		    (integerp value))
	       (call-next-method (find-object (slot-definition-foreign-type slotd) value) class object slotd))
	      ((and foreign-object-slots
		    *database*)

	       (map nil 
		    (lambda (slot)	   
		      (let ((fo (find-object (slot-definition-foreign-type slot) value)))
			(setf (slot-value-using-class class object slot) fo)))
		    foreign-object-slots)))))))

(defun find-foreign-objects (db-object)
    (let* ((class (class-of db-object)) 
	 (foreign-objects ))
      (mapcar (lambda (x)
		(and (slot-value-using-class class db-object x)  
		     (slot-value-using-class class db-object  x)))
	      (remove-if-not #'slot-definition-foreign-type 
			     (class-slots class)))))


(defun class-id-column-name (class)
  (slot-definition-column-name
   (or (class-id-slot-definition class)
       (error "No ID slot (primary key) for ~A" class))))



(defun primary-key-boundp (object)
  (check-type object standard-db-access-object) 
  (slot-boundp object (class-id-column-name (class-of object))))


(defmacro %initialize-standard-db-access-class ()
  `(let ((direct-slots (loop for slot in direct-slots 
			 collect (let* ((sname (getf slot :name))
					(readers (getf slot :readers))
					(writers (getf slot :writers)))
				   (setf (getf slot :readers)
					 (cons (intern (format nil "~A.~A"
							       name sname)) readers))
				   (setf (getf slot :writers)
					 (cons `(setf ,(intern (format nil "~A.~A"
								       name sname))) writers))
				   slot))))
	 


    (if (or (eq name 'standard-db-access-object)
	    (loop for direct-superclass in direct-superclasses
	       thereis (ignore-errors (subtypep direct-superclass 'standard-db-access-object))))
	(call-next-method)
	(apply #'call-next-method
	       class
	       :direct-superclasses
	       (append direct-superclasses
		       (list (find-class 'standard-db-access-object)))
	       :direct-slots direct-slots
	       initargs))))

(defmethod initialize-instance :around ((class standard-db-access-class) 
					&rest initargs 
					&key name 
					     (direct-superclasses '()) 
					     direct-slots)
  (declare (dynamic-extent initargs))
  (%initialize-standard-db-access-class))

(defmethod reinitialize-instance :around ((class standard-db-access-class) 
					  &rest initargs 
					  &key (name (class-name class)) 
					       (direct-superclasses '() direct-superclasses-p) 
					       direct-slots)
  (declare (dynamic-extent initargs))
  (%initialize-standard-db-access-class))



  
