(defpackage :maxclaims/ecm-description/manage
  (:use))
  
(in-package :maxclaims/ecm-description)

(defclass manage-viewer () ())

(defun as-table (name &key (attributes nil attributes-provided?)
			(layers :manage))
  `(,name :as-table t
	  :limit 50
	  :offset query-offset
	  :layers ,layers
	  ,@(when attributes-provided?
		  (list :attributes attributes))))

(eval
 `(define-descriptions manage-viewer
    (:default identity)
    (:heading (:value :value "Manage ECM (Users and Examiners)"		      ))
    (:view )
    (:view-tab 
     (tab :label "Examiners"
	  :tab :adjusters)
     (tab :label "Active Users"
	  :tab :active-users)     
     (tab :label "Admins"
	  :tab :administrators)
     (tab :label "All Users"
	  :tab :all-users)
     (tab :label "\"Read Only\" Users"
	  :tab :read-only-users))
    (:read-only-users 
     (read-only-users 
      :as-table t
      :limit 25
      :offset query-offset
      :layers :manage))
    (:all-users 
     (all-users 
      :as-table t
      :limit 25
      :offset query-offset
      :layers :manage))

    (:adjusters
     ,(as-table 
       'adjusters
       :layers :default))

    (:administrators
     (administrators
      :as-table t
      :limit 25
      :offset query-offset
      :layers :manage))
    (:active-users 
     (active-users 
      :as-table t
      :limit 25
      :offset query-offset
      :layers :manage))))

(defmethod object-attribute-value ((tc manage-viewer) 
				   (a (eql 'all-users))
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::query-objects 
   'app-user
   (lambda (n fs)
     `(:limit 
       (:order-by 
	(:select 
	 ,@fs 
	 :from ,n)	  	  
	(:desc app-user-id))
       ,limit , (typecase offset
		  (symbol (funcall offset))
		  (T offset))))))

(defmethod object-attribute-value ((tc manage-viewer) 
				   (a (eql 'read-only-users))
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::query-objects
   'app-user
   (lambda (&rest _)
     (declare (ignore _))
     `(:limit 
       (:order-by 
	(:raw "
SELECT app_user.*
 FROM app_user 
 WHERE app_user_id IN (select app_user_id FROM app_user_contract)
 AND app_user.password IS NOT NULL 
 AND app_user.password != ''")
	(:desc app-user-id))
       ,limit , (typecase offset
		  (symbol (funcall offset))
		  (T offset))))))

(defmethod object-attribute-value ((tc manage-viewer) 
				   (a (eql 'active-users))
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::query-objects 
   'app-user
   (lambda (n fs)
     `(:limit 
       (:order-by 
	(:select 
	 ,@fs 
	 :from ,n 
	 :where (:or (:not (:= password ""))
		     (:not (:is-null password))))
	(:desc app-user-id))
       ,limit , (typecase offset
		  (symbol (funcall offset))
		  (T offset))))))

(defmethod object-attribute-value ((tc manage-viewer) 
				   (a (eql 'administrators))
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::query-objects 
   'app-user
   (lambda (n fs)
     `(:limit 
       (:order-by 
	(:select 
	 ,@fs 
	 :from ,n 
	 :where (:= admin t))
	(:desc app-user-id))
       ,limit , (typecase offset
		  (symbol (funcall offset))
		  (T offset))))))

(defmethod object-attribute-value ((tc manage-viewer) 
				   (a (eql 'adjusters))
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::query-objects 
   'app-adjuster
   (lambda (n fs)
     `(:limit 
       (:order-by 
	(:select 
	 ,@fs 
	 :from ,n)
	(:desc active)
	(:person-name person-id))
       ,limit , (typecase offset
		  (symbol (funcall offset))
		  (T offset))))))



