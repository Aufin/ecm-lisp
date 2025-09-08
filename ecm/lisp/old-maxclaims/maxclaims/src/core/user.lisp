(in-package :maxclaims)

(defgeneric user-can-delete-p (user))
(defgeneric user-can-edit-p (object))
(defgeneric user-enabled-p (user))
(defgeneric user-read-only-p (user))
(defgeneric user-is-adjuster-p (user))


(defgeneric user-contracts (user)
  (:documentation "Return list of CONTRACT objects a user is permitted
to view, or NIL for all are permissible"))

(defmethod user-contracts ((user app-user))
  ;; fixme: way too consy for the number of times this is called per
  ;; page view
  (let ((d (remove-duplicates (append (mapcar #'app-user-contract.contract
					      (app-user.contracts user))
				      (mappend (compose #'person.contracts-as-agency
							#'app-user-agency.agency)
					       (app-user.agency user)))
			      :test #'db=)))
    #+nil(break "contracts: ~A" d)
    d))

(defmethod user-can-delete-p (object)
  (app-user.admin $app-user))

(defmethod user-can-edit-p (object)
  (null (user-contracts $app-user)))

(defmethod user-can-edit-p ((object contract))
  (app-user.admin $app-user))

(defmethod user-read-only-p ((user app-user))
  (or (app-user.contracts user) (app-user.agency user)))

(defmethod user-is-adjuster-p (user)
  (and (slot-boundp user 'person-id)
       (select 'person-id 
	       :from 'app-adjuster
	       :where `(:= person-id
			   ,(app-user.person-id user)))))

(defmethod user-enabled-p ((user app-user))
  (and  (slot-boundp user 'password)
	(> (length (app-user.password user)) 1)))

(defun find-user (username password)
  (unless (or (equal "" username)
	      (equal "" password))
  (let ((user (with-db
		(select-only-n-objects 
		 1 'app-user 
		 :where `(:and (:= ,username username)
			       (:= ,password password))))))
    ;;(break "~A" user)
    (prog1 user
      (when user 
	(let* ((time (get-universal-time))
	       (name (format nil "~A-~A" time (app-user.username user))))
	  (setf (app-user.log user)
		(list name (bt:make-recursive-lock name) time))))))))

