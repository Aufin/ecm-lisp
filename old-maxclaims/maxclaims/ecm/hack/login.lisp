(defpackage :ecm/hack/login
  (:use :cl :ecm/request-user))

(in-package :maxclaims)

(defun login-user (username password)
  (maxclaims::with-adb 
    (let ((json (postmodern:query (:select (:login.login-user username password))
                                  :single)))
      (unless (eq :null json)
        (st-json:read-json-from-string json)))))
 

(defun maxclaims::find-user (username password)
  (let* ((l (login-user username password))
         (uid (when l (st-json:getjso "user_id" l)))
         (user (when uid
                 (maxclaims::with-adb
                   (maxclaims::select-only-n-objects
                    1 'app-user
                    :where `(:= ,uid app-user-id))))))
    (multiple-value-prog1 (values user l)
      (when user 
        (let* ((time (get-universal-time))
               (name (format nil "~A-~A" time (maxclaims::app-user.username user))))
          #+(or)(setf (maxclaims::app-user.log user)
                (list name (bt:make-recursive-lock name) time)))))))

(defun login-user-and-set-cookie? (username password)
  (multiple-value-bind (user login)
      (maxclaims::find-user username password)
    (when user
      (hunchentoot:set-cookie
       "ecm-login"
       :value (st-json:getjso "id" login)
       :path "/")
      (setf (hunchentoot:session-value :app-user) user)
      user))) 

(defmethod maxclaims::check-credentials (username password)
  (prog1 (login-user-and-set-cookie?  username password)
    (maxclaims::app-user-log
     :log-type (if maxclaims::$app-user "LOGIN SUCCESS" "LOGIN DISALLOWED")
     :log-info username)))

(setf (symbol-function 'old-hunchentoot-session-value)
      (symbol-function 'hunchentoot::session-value))

(defun old-session-value (symbol &optional (session hunchentoot::*session*))
  "Returns the value associated with SYMBOL from the session object
SESSION \(the default is the current session) if it exists."
  (when session
    (let ((found (assoc symbol (hunchentoot::session-data session) :test #'eq)))
      (values (cdr found) found))))

(defun hunchentoot::session-value (symbol &optional (session hunchentoot::*session*))
  (if (eq symbol :app-user)
      (or (old-session-value symbol session)
          (let ((u (ecm/request-user:request-user*)))
            (when u (setf (hunchentoot:session-value :app-user session) u)
                  u)))
      (old-session-value symbol session)))
