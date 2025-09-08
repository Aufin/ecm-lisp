(defpackage :ecm/request-user 
  (:use :cl)
  (:import-from :postmodern)
  (:import-from :ecm/database)
  (:export #:request-user #:request-user*))

(in-package :ecm/request-user)

(defun request-user-id (request)
  (maxclaims::with-adb
    (let* ((uuid (hunchentoot:cookie-in "ecm-login" request))
           (uid (when uuid
                  (postmodern:query "SELECT user_id FROM login.login
WHERE id = $1
AND login.is_active(id)" uuid :single))))
      uid)))

(defun request-user (request)
  (let ((uid (request-user-id request)))
    (when uid
      (maxclaims::with-adb
        (maxclaims::select-only-n-objects
         1 'maxclaims::app-user
         :where `(:= app-user-id , uid))))))

(defun request-user* (&optional (request hunchentoot:*request*))
  (request-user request))
