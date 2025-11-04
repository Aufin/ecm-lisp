(in-package :rofl)

;;;; * A PLIST reader for postmodern.	  
(postmodern::def-row-reader symbol-plist-row-reader (fields)
  (let ((symbols (map 'list (lambda (desc) 
		   (postmodern::from-sql-name (postmodern::field-name desc))) fields)))
    (loop :while (postmodern::next-row)
          :collect (loop :for field :across fields
                         :for symbol :in symbols
                         :nconc (list symbol (postmodern::next-field field))))))

(s-sql::def-sql-op :between (n start end)
  `(,@(s-sql::sql-expand n) " BETWEEN " ,@(s-sql::sql-expand start) " AND " ,@(s-sql::sql-expand end)))

(defclass described-db-access-class 
    (standard-db-access-class)
  ())

(defmethod c2mop:class-slots :around ((class standard-db-access-class))
  (let ((slots (ignore-errors (call-next-method))))
    (or slots (progn (finalize-inheritance class)
		     (call-next-method)))))

(defmethod c2mop:class-slots :around ((class described-db-access-class))
  (let ((slots (ignore-errors (call-next-method))))
    (or slots (progn (finalize-inheritance class)
		     (call-next-method)))))


(defgeneric slot-definition-primary-key-p (slotd)
  (:method (slotd) nil))

(defgeneric class-primary-keys (class)
  (:method (class)
    (remove-if-not #'slot-definition-primary-key-p 
		   (class-slots class))))

(defun class-id-slot-definition (class)
  (let ((slots (class-primary-keys class)))
    (if (not (second slots))
	(first slots)
	(error "~A class has more then one pkey" class))))

(defmethod delete-object ((object standard-db-access-object))
  (let* ((class (class-of object))
	 (pkeys (class-primary-keys class)))
    (delete-from 
     (class-table-name class) 
     :where `(:and 
	      ,@(loop for pk in pkeys collect 
		     `(:= ,(slot-definition-column-name pk)
			  ,(slot-value-using-class class object pk)))))))

;; This will be a reference. When the reference is encountered, a
;; number of things will happen.

(defstruct %ref id)

(defun make-object-from-plist (type plist &optional object)
  (let* ((class (find-class type))
	 (object (or object (make-instance class)))
	 (slotds (c2mop:class-slots class))
	 (initarg-test 
	   ;; The selects which key in the plist accounts for a slot
	   (lambda (key slotd)
	     (or (find key (slot-definition-initargs slotd))
		 (string= key (slot-definition-column-name slotd))
		 (string= key (slot-definition-name slotd))))))
    (loop 
      :for (key val) :on plist :by #'cddr 
      :do ;; This could be cached, but right now do it every time.
	 (dolist (slotd (remove key slotds :test-not initarg-test))	   
	    (if (and (slot-definition-foreign-type slotd)
		     (not (db-access-object-p val)))
		;; the slot is object related to the foreign key that
		;; is being set. We make it a reference to be retreived when needed.
	       (setf (slot-value-using-class class object slotd) 
		     (make-%ref :id val))		 
	       (setf (slot-value-using-class class object slotd) 
		     val)))
      :finally (return (funcall (if *instance-is-persistent* 
				    #'mark-instance-as-persistent
				    #'identity)
				object)))))

(defun find-object (type id 
		    &key (table (class-table-name (find-class type)))
		      id-column-name)
			     
  "Get the dao corresponding to the given primary key,
or return nil if it does not exist."
  (when id 
    (let ((plist 
	   (select-only 1 '* 
			:from table 
			:where (list ':= id (or id-column-name
						(class-id-column-name 
						 (find-class type)))))))
      (when plist  (make-object-from-plist type plist)))))

;; Debugging
#+(or)(when (typep object 'maxclaims::contract)
    (break "Set: CLASS ~A ~%OBject:~A~%slotd~A~%value: ~A" 
	   class object slotd value))

(defmethod (setf slot-value-using-class) :around 
    (value (class standard-db-access-class) object slotd)
  (let ((*previously-set-slots* (if *previously-set-slots* 
				    *previously-set-slots*
				    (make-hash-table :test #'eq))))
    ;; The *PREVIOUSLY-SET-SLOTS* thing is kind of ... odd
    ;; It is kept in because it is needed, but why it is needed is... odd.

    (unless (member slotd (gethash  object *previously-set-slots*))
      (push slotd (gethash  object *previously-set-slots*))
      (call-next-method)
      (let ((foreign-object-slots 
	     (remove-if-not 
	      (lambda (class-slot)
		(and (slot-definition-foreign-type class-slot)
		     (eq (slot-definition-column-name class-slot)
			 (slot-definition-column-name slotd))
		     (not (eq slotd class-slot)))) 
	      (class-slots class))))
	#+nil((and (eq value :null)
		    (slot-definition-foreign-type slotd))
		    (string-equal value "asd")
		    (break "~A" object)
	       (setf (slot-value object (slot-definition-column-name 
					 slotd))
		     :null))
	
	(cond  
	      ((and value 
		    (typep value 'standard-db-access-object)
		    (slot-definition-foreign-type slotd)
		    (primary-key-boundp value))
	       (set-foreign-keys-from-foreign-type-slotd value class object slotd))

	      ((and value 
		    (slot-definition-foreign-type slotd)
		    (or (integerp value)
			(stringp value)))
	       (call-next-method  (make-%ref :id value) class object slotd))
	      ((and foreign-object-slots
		    *database*)
	       (map nil 
		    (lambda (slot)		        
		      (setf (slot-value-using-class class object slot) 
			    (if (%ref-p value) value (make-%ref :id value))))
		    foreign-object-slots)))))))


(defmethod slot-value-using-class :around 
    ((class standard-db-access-class) object slotd)
  (cond ((slot-definition-foreign-relation slotd)
	 (if (slot-boundp-using-class class object slotd)
	     (call-next-method)
	     (setf (slot-value-using-class class object slotd) 
		   (find-foreign-relations class object slotd))))
	((slot-definition-foreign-type slotd)
	 (let ((val (call-next-method)))
	   (if (%ref-p val)
	       (progn  ;(break " val~A ~%00~A" val slotd)
		 (let ((obj 
			 (let ((*instance-is-persistent* t)
			       (id (%ref-id val)))
			   (if (equalp "" id)
			       nil
			       (find-object (slot-definition-foreign-type slotd)
					    id)))))
					    
		   #+(or)(when obj
		   (setf (slot-value-using-class class object slotd) 
			 obj))
		 obj))
	       val)))
	(t (call-next-method))))

(defmethod update-object ((object standard-db-access-object))
  ;; Update related tables *before* updating primary object in case
  ;; any CASCADEd columns are changed
  (ensure-transaction ()
    (update-foreign-objects object)
    (when (and (slot-boundp object '%persistent/modifications)
	       (slot-value object '%persistent/modifications))
     ; (break "~A" 'asd)
      (let* ((class (class-of object))
	     (update-query 
	      (apply #'nconc 
		     (remove-duplicates 
		      (loop 
			:for (slot &rest old-value) 
			:in (remove-duplicates (slot-value object '%persistent/modifications)
			     :key #'car)
			
			:collect (list (slot-definition-column-name (find slot (class-slots class) :key #'slot-definition-name))
				       (if (slot-boundp object slot)
					   (let ((val (slot-value object slot)))
					     (if (db-access-object-p val)
						 (object-id val)
						 val)) 
					   :NULL)))
		      
		      :key #'car)))
	     (pkeys (class-primary-keys class)))
	(apply #'update (class-table-name class) 
	       :set (nconc  update-query
			    (list  :where `(:and 
	      ,@(loop for pk in pkeys collect 
		     `(:= ,(slot-definition-column-name pk)
			  ,(slot-value-using-class class object pk)))))))))
    (setf (slot-value object '%persistent/modifications) '())
    object))


(defmethod insert-object ((object standard-db-access-object))
  (ensure-transaction ()
    (let* ((class (class-of object))
	  (db-slotds (class-db-slots class))
	  insert-query)
      (flet 
	  ((ins (slotd &optional 
		       (val (slot-value-using-class class object slotd)))
	     (push (slot-definition-column-name slotd) insert-query)
	     (push  val insert-query)))
	(loop :for slotd in db-slotds 
	   :do (cond 
		 
		 (;; * Is bound, so needs inserting
		  (slot-boundp-using-class class object slotd)		
		  (cond 
		    ((slot-definition-foreign-type slotd)
		     ;; ** It is a foreign-type
		     (ins slotd 
			  (let ((value 
				 (slot-value-using-class class object slotd)))
			    (if (and value
				     (typep value 'standard-db-access-object))
				(slot-value value (class-id-column-name (class-of value)))
				(or value :null)))))
		    ((slot-definition-foreign-relation slotd)
		     ;; ** It is a foreign relation
		     ;;    so a list of other objects, ignore
		     )
		    (t 
		     ;; ** Otherwise, insert!
		     (ins slotd))))
		 
		 (;; * Is primary key.
		  (and (slot-definition-primary-key-p slotd)
		       (not (slot-boundp-using-class class object slotd)))
		  ;; ** It is a primary key but unbound...

		  ;; (break "~A" (slot-definition-column-name slotd))

		  (let ((others 
			 ;; ** First, let us see if there are more
			 ;;    ... slots that have this pkey value
			 (remove-if-not 
			  (lambda (s)
			    (and (equal 
				  (slot-definition-column-name slotd)
				  (slot-definition-column-name s))
				 (not (eql s slotd))))
			  db-slotds)))
		   #+ (or) (break "~A" others)

		   ;; ** If there are none, insert the default value
		   (unless others 		      
		     (setf (slot-value-using-class class object slotd) 
			   (get-default-value (class-table-name class)
					      (slot-definition-column-name slotd)))
		     (ins slotd))))))
	(let ((query (loop 

			:for (key val) 
			:on (nreverse insert-query) 
			:by #'cddr
			:collect key into keys
			:unless (find key (butlast keys))
			:nconc (list key val) into list
			:finally (return list))))

	  (apply #'insert-into (class-table-name class) query))))
    (update-foreign-objects object)
    (mark-instance-as-persistent object)
    object))

(defmethod delete-foreign-object (dao (slot-name symbol) foreign-dao)
  (if-bind slotd (find slot-name (class-slots (class-of dao))
		       :key #'slot-definition-name)
    (when foreign-dao 
      (delete-foreign-object dao slotd foreign-dao))
    (error "Invalid slot name ~A for class ~A" slot-name (class-of dao))))
