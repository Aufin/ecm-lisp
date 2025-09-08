(in-package :maxclaims)

;; (define-mergeable-table table-class
;;   ((foreign-table :key key :foreign-keys (fkey0 fkey1 ...))
;;   ...))
;; Constraints are wrapped in () to permit future expansion with
;; defclass style options... (just in case)

(defmacro define-mergeable-table (table-class (&rest constraints))
  (let ((deleted-table-class (symbolicate 'deleted- table-class))
	(merge-class (symbolicate table-class '-merge))
	(table-class-id (rofl::class-id-column-name (find-class table-class)))
	(constraints (mapcar (lambda (constraint-slot)
			       (list (car constraint-slot)
				     (getf (cdr constraint-slot) :key)
				     (getf (cdr constraint-slot) :foreign-keys)))
			     constraints)))
    (with-gensyms (d m c)
      `(progn
	 (defclass ,deleted-table-class (deleted-record ,table-class)
	   ()
	   (:metaclass described-db-access-class))
	 
	 (defmethod deleted-record.class ((,d ,deleted-table-class))
	   (declare (ignore ,d))
	   ',table-class)

	 (defclass ,merge-class (merge-history)
	   ()
	   (:metaclass described-db-access-class))
	 
	 (defmethod merge.master-table ((,m ,merge-class))
	   (declare (ignore ,m))
	   ',(rofl::class-table-name (find-class table-class)))

	 (defmethod merge.deleted-record ((,m ,merge-class))
	   (select-only-n-objects 1 ',deleted-table-class
	     :where (list ':= ',table-class-id (merge-history.from-id ,m))))

	 (defmethod merge.constraints ((,m ,merge-class))
	   ',constraints)

	 (defmethod table-constraints ((,c ,table-class))
	   (declare (ignore ,c))
	   ',constraints)

	 (defmethod table-constraints ((,c (eql ',table-class)))
	   ',constraints)

	 (defmethod merge-class ((,c ,table-class))
	   (declare (ignore ,c))
	   ',merge-class)

	 (defmethod make-deleted-record ((,c ,table-class))
	   (apply #'make-object ',deleted-table-class
		  (rofl::make-insert-object-plist ,c)))))))

; (rofl::class-id-column-name (find-class 'contract))
