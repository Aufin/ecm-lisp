(eval-when (:compile-toplevel) (require :stefil))
(cl:defpackage rofl-test
  (:use :cl :stefil :rofl))


(in-package :rofl-test)

(declaim (optimize (debug 3)))
;;;; CREATE USER rofl_test PASSWORD 'rofl_test';
;;;; CREATE DATABASE rofl_test OWNER rofl_test;          xcvbn.,mnbvcxz       

(defsuite rofl)
(in-suite rofl)

(defmacro db (&body body)
 `(postmodern:with-connection '("rofl_test" "rofl_test" "rofl_test" "localhost")
    ,@body))
   
(deftest test-create-table ()
  (finishes (db 
    (ignore-errors (postmodern:query (:DROP-TABLE 'rofl_test_base)))

    (postmodern:query (:CREATE-TABLE rofl_test_base 
		       ((rofl_test_base_id :type SERIAL :primary-key t)
                        (test_string :type string) 
			(test_integer :type integer)))))))

(deftest test-simple-insert ()
  (test-create-table)
  (let ((plist '(test-string "Test Entry" test-integer 1)))
    (finishes (db
		(postmodern:execute 
		 (postmodern:sql-compile  `(:insert-into rofl-test-base :set ,@plist)))))))

(deftest test-rofl-select ()
  (test-simple-insert)
  (db 
  (finishes 
    (let* ((result (first (select '* :from 'rofl-test-base))))
      (is (equalp '(:ROFL-TEST-BASE-ID 1 :TEST-STRING "Test Entry" :TEST-INTEGER 1) result))))))

(deftest test-rofl-select-only-1 ()
  (test-simple-insert)
  (db 
  (finishes 
    (let* ((result (select-only 1 '* :from 'rofl-test-base)))
      (is (equalp '(:ROFL-TEST-BASE-ID 1 :TEST-STRING "Test Entry" :TEST-INTEGER 1) result))))))

(deftest test-rofl-insert ()
  (test-create-table)
  (db 
    (finishes (insert-into 'rofl-test-base :test-integer 2 :test-string "a"))
    (finishes (insert-into 'rofl-test-base :test-integer 3 :test-string "b"))
    (finishes (insert-into 'rofl-test-base :test-integer 4 :test-string "c"))
    
    (let ((r (select '* :from 'rofl-test-base)))
      (is (equal 3 (length r))))))

(deftest test-rofl-class-creation ()
  (finishes (eval '(progn 
		    (setf (find-class 'rofl-test-base) nil)
		    (defclass rofl-test-base ()
		      ((rofl-test-base-id :primary-key t)
		       (test-integer :initarg :test-integer) 
		       (test-string :initarg :test-string))
		      (:metaclass standard-db-access-class))))))


(deftest test-rofl-make-object-from-plist ()
  (test-rofl-class-creation)
  (let* ((plist '(:ROFL-TEST-BASE-ID 1 :TEST-STRING "a" :TEST-INTEGER 2))
	 (object (make-object-from-plist 'rofl-test-base plist)))
    (is (equal (slot-value object 'rofl-test-base-id) 1))))
    

(deftest test-rofl-select-objects ()
  (test-create-table)
  (test-rofl-class-creation)
  (test-rofl-insert)

  (db (finishes 
    (let ((objects (select-objects 'rofl-test-base  
				 :where '(:= rofl-test-base-id 1))))
      (is (equal (slot-value (first objects) 'rofl-test-base-id) 1))))))

(deftest test-rofl-create-references-tables ()
  (finishes 
    (db 
      (ignore-errors (postmodern:query (:DROP-TABLE 'rofl_test_child)))
      (ignore-errors (postmodern:query (:DROP-TABLE 'rofl_test_parent)))
      
      (postmodern:query (:CREATE-TABLE rofl_test_parent 
				       ((rofl_test_parent_id 
					 :type SERIAL 
				 	 :primary-key t)
					(test_string 
					 :type string) 
						(test_integer 
						 :type integer))))
    


	      (postmodern:query (:CREATE-TABLE rofl_test_child 
					       ((rofl_test_child_id 
						 :type SERIAL 
						 :primary-key t)
						(rofl_test_parent_id 
						 :type (or integer postmodern::db-null)
						 :references (rofl_test_parent)
						 )
						(test_string 
						 :type string) 
						(test_integer 
						 :type integer)))))))

(deftest test-rofl-references ()
  (test-rofl-create-references-tables)

  (defclass rofl-test-parent ()
    ((rofl-test-parent-id :primary-key t)
     (test-string :initform "test")
     (test-integer :initform 1))
    (:metaclass standard-db-access-class))

  (defclass rofl-test-child ()
    ((rofl-test-child-id :primary-key t)
     (rofl-test-parent-id)
     (parent :column rofl-test-parent-id
	     :references rofl-test-parent)
     (test-string :initform "test-child")
     (test-integer :initform 1234))
    (:metaclass standard-db-access-class))

  (defclass rofl-test-child-no-base-fkey ()
    ((rofl-test-child-id :primary-key t)
     (parent :column rofl-test-parent-id
	     :references rofl-test-parent)
     (test-string :initform "test-child")
     (test-integer :initform 1234))
    (:metaclass standard-db-access-class)
    (:table rofl-test-child))


  (db (let* ((parent (insert-object (make-object 'rofl-test-parent)))
	     (child (insert-object (make-object 'rofl-test-child 
				    :rofl-test-parent-id (rofl-test-parent.rofl-test-parent-id parent))))
	     (child1 (find-object 'rofl-test-child (rofl-test-child.rofl-test-child-id child))))
    (is (db-access-object-= parent (rofl-test-child.parent child)))
    (is (db-access-object-= child1 child))
    (is (db-access-object-= parent (rofl-test-child.parent child1))))))

(deftest test-db= ()
  (test-rofl-references)
  
  (db (let ((object (find-object 'rofl-test-parent 1)))
	(db= object object object object))))

(deftest test-foreign-type-to-foreign-key-propagate ()
  (test-rofl-references)
  (db (let* ((parent (insert-object (make-object 'rofl-test-parent)))
	     (parent2 (find-object 'rofl-test-parent (rofl-test-parent.rofl-test-parent-id parent)))
	     (child (insert-object (make-object 'rofl-test-child 
						:parent parent)))
	     (child1 (find-object 'rofl-test-child (rofl-test-child.rofl-test-child-id child)))
	     (parent3 (rofl-test-child.parent child1)))
	(is (db= parent parent2 parent3))
	(is (db= child1 child)))))
  
(deftest test-persistent-instance ()
  (test-rofl-class-creation)
  (let ((instance (make-object 'rofl-test-base :test-string "asd" :test-integer 1)))
    ;; insert will mark as persistent.
    (is (null (persistentp instance)))
    (db (insert-object instance))
    (is (not (null (persistentp instance))))

    (is (not (modifiedp instance)))
    (setf (rofl-test-base.test-string instance) "zxc")
    (is (modifiedp instance)) 
    instance))

(deftest test-slot-modification-tracking ()
  (db (let ((instance (insert-object (make-object 'rofl-test-base :test-string "asd" :test-integer 1))))
    
    (is (notany #'(lambda (s) (slot-modified-p instance s))
		(rofl::class-slots (class-of instance))))
    (setf (rofl-test-base.test-string instance) "foo")
    (dolist (slotd (rofl::class-slots (class-of instance)))
      (if (eq (rofl::slot-definition-name slotd) 'test-string)
	  (is (slot-modified-p instance slotd))
	  (is (not (slot-modified-p instance slotd)))))
    instance)))
