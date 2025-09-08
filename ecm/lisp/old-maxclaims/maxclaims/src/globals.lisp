(in-package :maxclaims)

(defvar *debug-on-error* NIL)
(define-symbol-macro $window 
    (context.window-component *context*))

(define-symbol-macro $body 
    (current-component $window))

(defvar %*app-user*% nil)

(defun call-with-app-user (user fn)
  (let ((%*app-user*% user))
    (funcall fn)))

(defun %app-user ()
  (or %*APP-USER*% (hunchentoot:session-value :app-user)))

(defun (setf %app-user) (user)
  (setf (hunchentoot:session-value :app-user) user))

(define-symbol-macro $app-user
    (%app-user))

(defun <link-to-viewer (object &key (link-text-fn 
				     (lambda ()
				       (<:as-html "View"))))
  (let* ((name (string-downcase (class-name (class-of object))))
	 (pkeys (remove-if-not #'rofl::slot-definition-primary-key-p 
			       (c2mop:class-slots (class-of object))))
	 (value (ignore-errors (c2mop:slot-value-using-class 
				(class-of object) object (first pkeys)))))
    (<:a :href (format nil "/maxclaims/view?view=t&~A=~A" name value)
	 (funcall link-text-fn))))
