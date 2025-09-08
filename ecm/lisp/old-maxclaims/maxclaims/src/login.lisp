(in-package :maxclaims)

(defmethod check-credentials (username password)
  (let ((user (with-adb (find-user username password))))
    (unless (and user (app-user.login user))
      (setf user nil))
    (setf (hunchentoot:session-value :app-user) user)
    (setf (hunchentoot:session-max-time hunchentoot:*session*)
	  (* 6 3600))
    (app-user-log  
     :log-type (if $app-user "LOGIN SUCCESS" "LOGIN DISALLOWED")
     :log-info (format nil "~A | ~A" username password))
    
    user))

(defun logout-user (&optional (session hunchentoot:*session*))
  (setf (hunchentoot:session-value :app-user) NIL)
  (hunchentoot:remove-session session))
