(in-package :maxclaims)

(define-description app-user-message (description-for-app-user-message)
  ((from
    :active :when :deactivate (editable) :attributes (username person)) 
   (to 
    :active :when :deactivate (editable) :attributes (username person))
   (subject )
   (message :function 'identity
	    :editp nil
	    :label "HERE")
   body
   unread
   (active-attributes :value '(from to
			       subject ))))

(define-description app-user-message (description-for-app-user-message)
  ((active-attributes :value '(from to 
			       (message 
				:attributes (subject)
				:activate (link-to-viewer)))))
  (:in-description inline))

(defun find-self (self)
  (make-instance 'app-user-message))
