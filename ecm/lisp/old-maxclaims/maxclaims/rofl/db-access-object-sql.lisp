(in-package :rofl)

(defvar *instance-is-persistent* nil)

(defgeneric insert-object (object))
(defgeneric update-object (object))
(defgeneric delete-object (object))

(defun %query-objects (type query-fn)
  (let* ((class (find-class type))
	 (fields (remove-duplicates 
		  (mapcar #'slot-definition-column-name 
			  (class-db-slots class)))))
    (let ((*instance-is-persistent* t))
      (mapcar (curry 'make-object-from-plist type)
	      (funcall query-fn (class-table-name class)
		       fields)))))

(defun query-objects (type query-fn &rest parameters)
  (%query-objects type (lambda (table fields)
			 (%query (funcall query-fn table fields) parameters))))

(defun %select-objects (type select-fn query)
  (let* ((class (find-class type))
	 (fields (remove-duplicates 
		  (mapcar #'slot-definition-column-name 
			  (class-db-slots class)))))
    
    (mapcar (curry 'make-object-from-plist type)
	    (apply select-fn  
		   (nconc fields  
			  (if (string-equal (first query) :from)
			      query
			      (append `(:from ,(class-table-name class)) query)))))))

(defun select-objects (type &rest query)
  (let ((*instance-is-persistent* t)) 
    (%select-objects type #'select query)))

(defun select-only-n-objects* (n offset type &rest query)
    (let ((*instance-is-persistent* t)) 

      (let ((fields (if (eq :fields (car query))
			(loop 			 :for cons :on (cdr query)
			 :if (not (keywordp (car cons)))
			 :collect (car cons) into fields
			 :else :do  
		         (setf query cons)
		         (return (nreverse (print fields)))
			 :finally 		       
		         (setf query cons)
		         (return (nreverse (print fields))))		       
			(list (intern "*")))))
	(let ((results 
	       (%query 
		`(:limit (:select 
				 ,@fields 
				 ,@(if (string-equal (first query) :from)
				       (print query)
				       (append `(:from ,type) query)))
			 ,n ,offset))))
	  (if (eql 1 n)
	      (when (first results) 
		(make-object-from-plist type (first results)))
	      (mapcar (curry 'make-object-from-plist type) results))))))

(defun select-only-n-objects (n type &rest query)
    (apply #'select-only-n-objects* n nil type query))

(defun make-object-from-plist (type plist &optional object)
  (let* ((class (find-class type))
	 (object (or object (make-instance class)))
	 (slotds (class-slots class)))
    (loop 
       :for (key val) :on plist :by #'cddr 
       :do 
         
	 (dolist (slotd (remove key slotds 
				:test-not (lambda (key slotd)
					    (or (find key (slot-definition-initargs slotd))
						(string= key (slot-definition-column-name slotd))
						(string= key (slot-definition-name slotd))))))
						
	     (setf (slot-value-using-class class object slotd) 
		   (if (and (slot-definition-foreign-type slotd)
			    (not (db-access-object-p val)))
		       (if 
			   val
			   (find-object (slot-definition-foreign-type slotd) 
					val))
		       val)))
       :finally (return (funcall (if *instance-is-persistent* 
				     #'mark-instance-as-persistent
				     #'identity)
				 object)))))

(defun make-object (type &rest plist)
  (make-object-from-plist type plist))

(defun reload-object (object)
  "Reload object from database, discarding any uncommited changes"
  (when (persistentp object)
    (setf (slot-value object '%persistent/modifications) '())
    (let ((updated-object (make-object-from-plist 
			   (class-name (class-of object)) 
			   (select-only 1 '*
					:from (class-table-name (class-of object))
					:where `(:= ,(class-id-column-name
						      (class-of object))
						    ,(object-id object))))))
      (dolist (s (class-db-slots (class-of object)))
	(cond ((slot-definition-foreign-relation s)
	       (slot-makunbound-using-class (class-of object) object s))
	      ((slot-boundp updated-object (slot-definition-name s))
	       (setf (slot-value object (slot-definition-name s))
		     (slot-value updated-object (slot-definition-name s))))
	      (t (slot-makunbound object (slot-definition-name s)))))))
  object)

(defun make-insert-object-plist (object)
 (let ((class (class-of object))
	insert-query)
    (flet 
     ((ins (slotd 
	    &optional 
	    (val 
	     (slot-value-using-class class object slotd)))
	(push (slot-definition-column-name slotd) insert-query)
	(push  val insert-query)))

      (loop :for slotd in (class-slots class) 
	    :do  (cond 
		  ((slot-boundp-using-class class object slotd)
		
		   (cond 
		     ((slot-definition-foreign-type slotd)

			   (let ((value 
				  (slot-value-using-class class object slotd)))
			     (when value  (ins slotd (slot-value value (class-id-column-name (class-of value))))))
		    )
		   (t 
		    (ins slotd))))
		  ((slot-definition-primary-key-p slotd)
		     (setf (slot-value-using-class class object slotd) (get-default-value (class-table-name class)
						  (slot-definition-column-name slotd)))
		     (ins slotd ))))
      
    (let ((query (loop 
		  :for (key val) 
		  :on (nreverse insert-query) 
		  :by #'cddr
		  :collect key into keys
		  :unless (find key (butlast keys))
		  :nconc (list key val) into list
		  :finally (return list))))

      
      query))))

(defmethod insert-object ((object standard-db-access-object))
  (ensure-transaction ()
    (let ((class (class-of object))
	  insert-query)
      (flet 
	  ((ins (slotd &optional 
		       (val (slot-value-using-class class object slotd)))
	     (push (slot-definition-column-name slotd) insert-query)
	     (push  val insert-query)))
	(loop :for slotd in (class-db-slots class) 
	   :do (cond 
		 ((slot-boundp-using-class class object slotd)
		  (cond 
		    ((slot-definition-foreign-type slotd)
		     (ins slotd 
			  (let ((value 
				 (slot-value-using-class class object slotd)))
			    (when (and value
				       (typep 'standard-db-access-object value))
			      (slot-value value (class-id-column-name (class-of value)))))))
		    ((slot-definition-foreign-relation slotd)
		     ;; ignore
		     )
		    (t 
		     (ins slotd))))
		 ((slot-definition-primary-key-p slotd)
		  (setf (slot-value-using-class class object slotd) 
			(get-default-value (class-table-name class)
					   (slot-definition-column-name slotd)))
		  (ins slotd))))
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

(defmethod delete-object ((object standard-db-access-object))
  (let ((class (class-of object)))
    (delete-from (class-table-name class) :where `(:= ,(class-id-column-name class)
						      ,(object-id object)))))

(defmethod update-object ((object standard-db-access-object))
  #+nil(break "update: ~A" object)
  ;; Update related tables *before* updating primary object in case
  ;; any CASCADEd columns are changed
  (ensure-transaction ()
    (update-foreign-objects object)
    (when (and (slot-boundp object '%persistent/modifications)
	       (slot-value object '%persistent/modifications))
      (let* ((class (class-of object))
	     (update-query 
	      (loop 
		 :for (slot &rest old-value) 
		 :in (remove-duplicates (slot-value object '%persistent/modifications) 
					:key #'car)
		 :nconc (list (slot-definition-column-name (find slot (class-slots class) :key #'slot-definition-name))
			      (if (slot-boundp object slot)
				  (let ((val (slot-value object slot)))
				    (if (db-access-object-p val)
					(object-id val)
					val)) 
				  :NULL)))))
	(apply #'update (class-table-name class) 
	       :set (nconc  update-query
			    (list  :where `(:= ,(class-id-column-name class)
					       ,(slot-value object (class-id-column-name class))))))))
    (setf (slot-value object '%persistent/modifications) '())
    object))

(defgeneric select-using-object-where-clause (search-value slotd column-name)
  (:method (search-value slotd column-name)
    `(:= ,column-name ,search-value)))

(defmethod select-using-object-where-clause ((search-value string) slotd column-name)
  `(:ilike ,column-name ,(format nil "~A%" search-value)))

;;; add generic (val) -> where-clause
(defun select-using-object (object &key (combinator :and))
  (let ((class (class-of object))
	select-query)
    (flet ((sel (slotd &optional (val (slot-value-using-class class object slotd)))
	     (unless (equal val "")
	       (push (select-using-object-where-clause
		    val slotd (slot-definition-column-name slotd))
		   select-query))))
    (loop :for slotd in (class-db-slots class) 
	  :do (cond ((slot-boundp-using-class class object slotd)
		     (unless (or (slot-definition-foreign-relation slotd)
				 (slot-definition-foreign-type  slotd))
		       (sel slotd)))))
    (if select-query
	   (select-objects (class-name class) 
	     :where `(,combinator ,@(nreverse select-query)))
	   nil))))

(defun get-default-value-query (table column)
  (format
   nil "select ~A "
   (second (select-only
            1 '(:pg_get_expr adbin adrelid)
            :from 'pg_attribute 'pg_attrdef
            :where `(:and (:= adnum attnum)
                          (:= attname ,(s-sql::to-sql-name column))
                          (:= adrelid attrelid)
                          (:= attrelid
                              (:limit
                               (:select
                                oid
                                :from pg_class
                                :where (:and
                                        (:= relname ,(s-sql::to-sql-name table)))) 1)))))))

(defun get-default-value (table column)
  (caar (query (get-default-value-query table column))))

(defvar *find-object-cache* nil)

(defun add-object-to-cache (type id object)
  nil)

(defun find-object-in-cache (type id)
  nil)


(defun funcall-with-find-object-cache (thunk)
  (funcall thunk))


  

			       
(defgeneric object-id (dao)
  (:method ((dao standard-db-access-object))
    (let ((class (class-of dao)))
      
      (slot-value-using-class class dao (class-id-slot-definition class)))))

(defun make-dao-from-row (type row &key slots)
  (let* ((class (find-class type))
	 (dao (make-instance class))
	 (slotds (class-slots class)))
    (loop 
	 :for val :in row 
	 :for slotd 
       :in (or 
	    (loop 
	       :for slot :in slots 
	       :collect (find slot slotds 
			      :key #'slot-definition-name))
	    slotds)
	 :do (setf (slot-value-using-class class dao slotd) val)
	 :finally (return (reinitialize-instance dao)))))
