(in-package :rofl)

(defclass standard-db-access-object ()
  ((%persistent/modifications :transient t)
   (%foreign-modifications :initform (list) :transient t))
  (:metaclass standard-db-access-class))

(defgeneric db-access-object-= (db-object1 db-object2)
  (:method ((db-object1 standard-db-access-object)(db-object2 standard-db-access-object))
    (and (equal (object-id db-object1) 
		(object-id db-object2))
	 (equal (class-table-name (class-of db-object1))
		(class-table-name (class-of db-object2)))))
  (:method (anything-else anything-else2) NIL))

(defun db= (&rest args)
  (reduce #'db-access-object-= args))

(defgeneric db-access-object-p (db-object)
  (:method ((db-object standard-db-access-object)) T)
  (:method (anything-else) NIL))

(defun persistentp (object)
  (and (db-access-object-p object)
       (slot-boundp object '%persistent/modifications)))

(defun mark-instance-as-persistent (db-object)
  (unless (persistentp db-object)
    (setf (slot-value db-object '%persistent/modifications) '()))
  db-object)

(deftype persistent-db-access-object () `(satisfies persistentp))

(defun record-modification (db-object slotd &optional (old-value nil slot-was-boundp))
  (let ((alist (slot-value db-object '%persistent/modifications)))
    (setf (slot-value db-object '%persistent/modifications)
	  (acons (slot-definition-name slotd) 
		 (when slot-was-boundp
		   (list old-value))
		 alist))))

(defun modifiedp (db-object)
  (and (persistentp db-object)
       (not (null (slot-value db-object '%persistent/modifications)))))

(defgeneric slot-modified-p (object slot)
  (:method ((object standard-db-access-object) (slot-name symbol))
    (and (modifiedp object)
	 (assoc slot-name (slot-value object '%persistent/modifications))))
  (:method ((object standard-db-access-object) (slotd standard-effective-slot-definition))
	    (slot-modified-p object (slot-definition-name slotd))))


(defmethod (setf slot-value-using-class) :around 
    (value class (object standard-db-access-object) slotd)
  (when (not (eq :NULL value))
    (when (and (persistentp object)
	       (not (slot-definition-foreign-relation slotd))
	       (not (slot-definition-transient-p slotd)))
      (if (slot-boundp-using-class class object slotd)
	  (record-modification object slotd (slot-value-using-class class object slotd))
	  (record-modification object slotd)))
	(call-next-method)))

(defmethod slot-makunbound-using-class :around 
    (class (object standard-db-access-object) slotd)
  (when (and (persistentp object)
	       (not (slot-definition-foreign-relation slotd))
	       (not (slot-definition-transient-p slotd)))
      (if (slot-boundp-using-class class object slotd)
	  (record-modification object slotd (slot-value-using-class class object slotd))
	  (record-modification object slotd)))
  (call-next-method))

(defmethod shared-initialize :after ((dao standard-db-access-object) 
				     slots &rest initargs)
  (let ((class (class-of dao))
	(foreign-key))
    (dolist (slotd (class-slots class))
      (with-slots (foreign-type) slotd
	(when foreign-type
	  (when (consp foreign-type)
	    (setf foreign-key (cdr foreign-type)
		  foreign-type (car foreign-type)))
	  (if (slot-boundp-using-class class dao slotd)
	      (let ((value (slot-value-using-class class dao slotd)))				
		(unless (typep value foreign-type)
		  (if (connected-p *database*)
		      (setf (slot-value-using-class class dao slotd)
			    (find-object foreign-type value))
		      (let ((obj (make-instance foreign-type)))
			(setf (slot-value-using-class 
			       (class-of obj)
			       obj
			       (class-id-slot-definition (class-of obj)))
			      value)))))))))))

(defmethod print-object ((object standard-db-access-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (ignore-errors
      (format stream ":~A ~S"
	      (string-downcase (symbol-name (class-id-column-name (class-of object))))
	      (object-id object)))))

