(in-package #:maxclaims.test)

(defsuite (duplicate-merge :in maxclaims))

(deftest (merge-test :in duplicate-merge) ()
  (labels ((test-merge/type (type pkey)
	     ;; Grab constraints for type
	     (dolist* ((table key (&rest fkeys))
		       (maxclaims::table-constraints type))
	       (declare (ignorable key))
	       ;; for each table+fkey, fetch three random fkey->type
	       (dolist (fkey fkeys)
		 (handler-case 
		     (destructuring-bind (a b c)
			 (query-objects
			  type
			  (lambda (ptable fields)
			    (let ((ids (query (:limit
					       (:select
						(:dot 'foo pkey)
						:distinct
						:from (:as
						       (:order-by
							(:select
							 (:dot type pkey)
							 :from type :inner-join table :on (:= (:dot type  pkey)
											      (:dot table fkey)))
							(:random))
						   
						       'foo))
					       3)
					      :column)))
			      `(:select ,@fields :from ,ptable
					:where (:in ,pkey
						    (:set
						     ,@ids)))))) 
		       ;; grab list of records to be modified (A -> B)
		       (let ((records-to-merge (maxclaims::find-records-to-merge a)))
			 (with-transaction* (test-transaction)
			   ;; merge
			   ;; ensure all records were affected
			   (multiple-value-bind (merge records-merged)
			       (maxclaims::merge-duplicate-records
				:from a :into b)
			     (is (results-equal records-to-merge
						records-merged))
			     ;; unmerge and ensure all records were unmerged
			     ;; - this implies that merge records were written
			     ;;   for each changed record
			     (is (results-equal
				  records-to-merge
				  (progn (maxclaims::unmerge-duplicate-record merge)
					 (maxclaims::find-records-to-merge a)))))
			   (abort-transaction test-transaction)))
		       ;; merge A -> B -> C
		       (with-transaction* (test-transaction)
			 (let ((records-to-merge/a->b
				(maxclaims::find-records-to-merge a))
			       (records-to-merge/b->c
				(maxclaims::find-records-to-merge b)))
			   (multiple-value-bind (merge/a->b merged-records/a->b)
			       (maxclaims::merge-duplicate-records
				:from a :into b)
			     (let ((records-to-merge/a->b->c
				    (maxclaims::find-records-to-merge b)))
			       (multiple-value-bind (merge/b->c merged-records/b->c)
				   (maxclaims::merge-duplicate-records
				    :from b :into c)
				 (is (results-equal records-to-merge/a->b->c
						    merged-records/b->c))
				 ;; Unmerge A <- B & verify
				 (is (results-equal
				      records-to-merge/a->b
				      (progn (maxclaims::unmerge-duplicate-record
					      merge/a->b)
					     (maxclaims::find-records-to-merge a))))
				 ;; Unmerge B <- C & verify
				 (is (results-equal
				      records-to-merge/b->c
				      (progn (maxclaims::unmerge-duplicate-record
					      merge/b->c)
					     (maxclaims::find-records-to-merge b))))
				 ;; verify & no records from A were modified. ))
				 (is (results-equal records-to-merge/a->b
						    (maxclaims::find-records-to-merge a)))))))
			 (abort-transaction test-transaction)))
		   (database-error (e)
		     (if (string= (database-error-code e)
				  "P0001")
			 ;; this will always be an ro-contract. just
			 ;; skip for now.
			 (break "skipping")
			 (error e)))))))
	   (results-equal (res1 res2)
	     ;; have to sort the keys lisp side
	     (if (or (null res1) (null res2))
		 (and (null res2) (null res2))
		 (let ((cur1 (car res1))
		       (cur2 (car res2)))
		   (and (equal (first cur1) (first cur2))
			(equal (second cur1) (second cur2))
			(equal (sort (copy-list (cddr cur1)) #'<)
			       (sort (copy-list (cddr cur2)) #'<))
			(results-equal (cdr res1) (cdr res2)))))))
    
    (maxclaims::with-db
      (test-merge/type 'maxclaims::contract 'maxclaims::contract-id)
      (test-merge/type 'maxclaims::person 'maxclaims::person-id))))
    