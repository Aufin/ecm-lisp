(defpackage :maxclaims/ecm-description/app-user-message
  (:use :maxclaims/data-entity/app-user-message)
  (:export))

(in-package :maxclaims/ecm-description)

;; http://localhost:8181/maxclaims/ecm/create?create[type]=app-user-message&create[key]=from-id&access[type]=app-user&access[id]=1&access[key]=app-user-id

(define-descriptions app-user-message 
  (:default 	app-user-message-id
		parent-message-id
		parent-message 
		(from :label t)
		
		from-id
		(to :label t)
		to-id
		subject
		body
		unread)
  (:heading from to)
  (:view subject body unread)
  (:view-tab (tab 
	      :label "Reply"
	      :tab replies
	      :active t))
  (replies (replies 
	    :label t
	    :active t
	    :as-table (:create app-user-message
			       parent-message-id)))
  (:create-heading (:value :label "Create"
			   :value "Message to ECM User"
			   :activate ((h3))))
  (:inline to subject body unread)
  
  (:create (from :label t
		 :edit nil)
	   (to :label "Message To"
	       :required t
	       :active t
	       :type app-user
	       :select-objects app-user)
	   (subject :label t
		    :required t
		    :active t)
	   (body :label "Message"
		 :textarea (:rows 10)
		 :active t)
	   #+ (or) (parent-message 
	    :label "In Reply To"
	    :active :when 
	    :edit nil)))

(defmethod object-attribute-value ((aum app-user-message) 
				   (a (eql 'from)) 
				   &key &allow-other-keys)
  (let ((v (call-next-method)))
;  (break "~A" v)
    (etypecase v
      (integer (maxclaims::find-object 'app-user v))
      (t v))))

(defmethod object-attribute-value ((aum app-user-message) 
				   (a (eql 'replies)) 
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::query-objects 
    'app-user-message  
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from 'app-user-message 
	  :where  
	  (:=
	   parent-message-id
	   ,(slot-value 
	     aum 'maxclaims/data-entity/app-user-message:parent-message-id)))
	 (:desc app-user-message-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset))))))

