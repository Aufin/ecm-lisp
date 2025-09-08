(in-package #:rofl)

;;; Modification of :referenced-from slots

;;; Not the best, but the alternatives didn't seem any better at the
;;; time

;;; Objects stored within %foreign-modifications
;;; :insert is always non-persistent
;;; :update is always persistent
;;; :delete is always persistant

;;; TODO
;;; - How should slot-modified-p behave with :referenced-from slots?
;;; - Should modify-foreign-object use the result of MUTATOR, or
;;;   instead require everything be done via side effects

;;; - Would a general stored command list that could be executed
;;;   within a single transaction (given the constraints with
;;;   continuations when used with ucw) be better than this?

(defgeneric insert-foreign-object (dao slotd foreign-dao))
(defgeneric update-foreign-object (dao slotd foreign-dao))
(defgeneric delete-foreign-object (dao slotd foreign-dao))

(defmethod insert-foreign-object (dao (slot-name symbol) foreign-dao)
  (if-bind slotd (find slot-name (class-slots (class-of dao))
		       :key #'slot-definition-name)
    (insert-foreign-object dao slotd foreign-dao)
    (error "Invalid slot name ~A for class ~A" slot-name (class-of dao))))

(defmethod update-foreign-object (dao (slot-name symbol) foreign-dao)
  (if-bind slotd (find slot-name (class-slots (class-of dao))
		       :key #'slot-definition-name)
    (update-foreign-object dao slotd foreign-dao)
    (error "Invalid slot name ~A for class ~A" slot-name (class-of dao))))

(defmethod delete-foreign-object (dao (slot-name symbol) foreign-dao)
  (if-bind slotd (find slot-name (class-slots (class-of dao))
		       :key #'slot-definition-name)
    (delete-foreign-object dao slotd foreign-dao)
    (error "Invalid slot name ~A for class ~A" slot-name (class-of dao))))

(defun table-name-compatible-with-slot (slotd dao)
  (equal (class-table-name (find-class (slot-definition-foreign-relation slotd)))
	 (class-table-name (class-of dao))))

(defun foreign-relations-makunbound (object)
  #+nil(break "resetting foreign relations for ~A" object)
  (when (persistentp object)
    (dolist (s (class-slots (class-of object)) object) 
      (if (slot-definition-foreign-relation s)
	  (slot-makunbound-using-class (class-of object) object s)))))

(defun foreign-slot-insert! (slotval foreign-dao)
  (pushnew foreign-dao slotval))

(flet ((recklessly-db= (dao1 dao2)
	 (if (and (persistentp dao1) (persistentp dao2))
	     (db= dao1 dao2)
	     (eql dao1 dao2))))

  ;; binding obsolete-dao to foreign-dao by default allows code to be
  ;; shared with apply-pending-foreign-updates: the db= comparison
  ;; will always be taken when called from there which will perform
  ;; the expected operation
  (defun foreign-slot-update! (slotval foreign-dao &optional (obsolete-dao foreign-dao))
    (nsubst foreign-dao obsolete-dao
	    slotval
	    :test #'recklessly-db=))

  (defun foreign-slot-delete! (slotval foreign-dao)
    (delete foreign-dao slotval
	    :test #'recklessly-db=)))

(defmethod insert-foreign-object ((dao standard-db-access-object)
				  (slotd db-access-slot-definition)
				  (foreign-dao standard-db-access-object))
  (assert (not (persistentp foreign-dao))
	  (dao slotd foreign-dao)
	  "Foreign Instance must not be persistent ~A" foreign-dao)
  (assert (table-name-compatible-with-slot slotd foreign-dao)
	  (dao slotd foreign-dao)
	  "Table name of foreign instance ~A (slot ~A) incompatible with instance ~A"
	  dao slotd foreign-dao)
  (with-slots ((%fm %foreign-modifications))
      dao
    (pushnew (make-foreign-modification
	      :operation :foreign-insert
	      :instance foreign-dao
	      :slotd slotd)
	     %fm :test #'equal)
    (setf (slot-value dao (slot-definition-name slotd))
	  (foreign-slot-insert! (slot-value dao (slot-definition-name slotd))
				       foreign-dao))
    #+nil(break "after insert: ~A" (slot-value dao (slot-definition-name slotd)))
    foreign-dao))

(defmethod update-foreign-object ((dao standard-db-access-object)
				  (slotd db-access-slot-definition)
				  (foreign-dao standard-db-access-object))
  (assert (table-name-compatible-with-slot slotd foreign-dao)
	  (dao slotd foreign-dao)
	  "Table name incompatible with slot: ~A ~A ~A" dao slotd foreign-dao)
  ;; fixme: ensure that fkey actually points to dao
  (with-slots ((%fm %foreign-modifications))
      dao
    (cond ((persistentp foreign-dao)
	   (assert (persistentp dao)
		   (dao)
		   "Cannot update persistent foreign reference ~A to non-persistent object ~A"
		   foreign-dao dao)
	   (if-bind obsolete-dao (find foreign-dao
				       (slot-value dao
						   (slot-definition-name slotd))
				       :test (lambda (dao1 dao2)
					       (ignore-errors (db= dao1 dao2))))
	     (progn (setf (slot-value dao (slot-definition-name slotd))
			  (foreign-slot-update!
			   (slot-value dao (slot-definition-name slotd))
			   foreign-dao obsolete-dao))
		    (pushnew (make-foreign-modification
			      :operation :foreign-update
			      :instance foreign-dao
			      :slotd slotd)
			     %fm :test #'equal))
	     (error "foreign-dao not found")))
	  ((not (persistentp foreign-dao))
	   ;; do nothing if the nonpersistant instance has already
	   ;; been inserted--insert-object will handle everything
	   ;; properly
	   (unless (find foreign-dao %fm
			 :test (lambda (o maybe-insert)
				 (when (eq (fm-operation maybe-insert)
					   :foreign-insert)
				   (eql o (fm-instance maybe-insert)))))
	     (error
	      "Foreign reference ~A not previously inserted in object ~A (~A)"
	      foreign-dao dao (slot-value dao (slot-definition-name slotd))))))
    foreign-dao))

(defun modify-foreign-object (dao slotd foreign-dao mutator)
  "Call UPDATE-FOREIGN-OBJECT with the result of (FUNCALL MUTATOR FOREIGN-DAO)"
  (update-foreign-object dao slotd (funcall mutator foreign-dao)))

(defmethod delete-foreign-object ((dao standard-db-access-object)
				  (slotd db-access-slot-definition)
				  (foreign-dao standard-db-access-object))
  (assert (table-name-compatible-with-slot slotd foreign-dao)
	  (dao slotd foreign-dao)
	  "Table name incompatible with slot: ~A ~A ~A" dao slotd foreign-dao)
  ;; fixme: ensure that fkey actually points to dao
  (with-slots ((%fm %foreign-modifications))
      dao
    (cond ((persistentp foreign-dao)
	   (assert (persistentp dao)
		   (dao)
		   "Cannot delete persistent foreign reference ~A to non-persistent object ~A"
		   foreign-dao dao)
	   (if (find foreign-dao
		     (slot-value dao (slot-definition-name slotd))
		     :test (lambda (dao1 dao2)
			     (ignore-errors (db= dao1 dao2))))
	       (prog1 (setf (slot-value dao (slot-definition-name slotd))
			    (foreign-slot-delete!
			     (slot-value dao (slot-definition-name slotd))
			     foreign-dao))
		 (pushnew (make-foreign-modification
			   :operation :foreign-delete
			   :instance foreign-dao
			   :slotd slotd)
			  %fm :test #'equal))
	     (error "foreign-dao not found")))
	  ((not (persistentp foreign-dao))
	   ;; Remove the insert operation rather than leaving
	   ;; operations to insert/delete on reload (simplifies other
	   ;; code -- reload can assume all :delete operations are
	   ;; persistent instances)
	   (if-bind inserted-fm-op
	       (find foreign-dao %fm
		     :test (lambda (o maybe-insert)
			     (when (eq (fm-operation maybe-insert)
				       :foreign-insert)
			       (eql o (fm-instance maybe-insert)))))
	     (prog1 (setf (slot-value dao (slot-definition-name slotd))
			  (foreign-slot-delete!
			   (slot-value dao (slot-definition-name slotd))
			   foreign-dao))
	       (setf %fm (remove inserted-fm-op %fm)))
	     (error
	      "Foreign reference ~A not previously inserted in object ~A (~A)"
	      foreign-dao dao (slot-value dao (slot-definition-name slotd))))))))

(defun update-foreign-objects (dao)
  #+nil (break "update: ~A" dao)
  (with-slots ((%fm %foreign-modifications))
      dao
    (flet ((insert-object* (insert-spec)
	     "Insert object and automatically relate to dao"
	     (let ((col (or (slot-definition-foreign-join-spec
			     (fm-slotd insert-spec))
			    (class-id-column-name (class-of dao))))
		   (foreign-dao (fm-instance insert-spec)))
	       (setf (slot-value foreign-dao col) (object-id dao))
	       (insert-object foreign-dao))))
      (mapc (lambda (foreign-operation)
	      (ecase (fm-operation foreign-operation)
		((:foreign-insert) (insert-object* foreign-operation))
		((:foreign-update) (update-object (fm-instance foreign-operation)))
		((:foreign-delete) (delete-object (fm-instance foreign-operation)))))
	    (nreverse (delete-duplicates %fm :test #'equal))))
    ;; Clear foreign modifications
    (setf (slot-value dao '%foreign-modifications) (list))
    (foreign-relations-makunbound dao)))

(defun apply-pending-foreign-operations (object slotd relations)
  "Performs any pending foreign operations on SLOTD when reloading a
slot after calling FOREIGN-RELATIONS-MAKUNOUND"
  (if-bind pending-operations (reverse
			       (remove slotd (and (slot-boundp object '%foreign-modifications)
						  (slot-value object '%foreign-modifications))
				       :test-not #'eql
				       :key #'fm-slotd))
    (progn
      (dolist (op pending-operations)
	(setf relations
	      (ecase (fm-operation op)
		((:foreign-insert) (foreign-slot-insert! relations
							 (fm-instance op)))
		((:foreign-update) (foreign-slot-update! relations
							 (fm-instance op)))
		((:foreign-delete) (foreign-slot-delete! relations
							 (fm-instance op))))))
      
      relations)
    relations))

(defmethod find-foreign-relations :around ((class db-access-class) object slotd)
  (apply-pending-foreign-operations object slotd (call-next-method)))