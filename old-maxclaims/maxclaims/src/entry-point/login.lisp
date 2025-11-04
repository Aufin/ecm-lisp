(defpackage :maxclaims/entry-point/login
  (:use :cl)
  (:import-from :maxclaims/entry-point/toplevel
		#:select-objects/key-value)
  (:import-from :maxclaims/web-display/html-page
		#:call-with-html-page
		#:with-user-html-page)
  (:import-from :maxclaims/web-display/display
		#:get-app-user
		#:display)
  (:import-from :maxclaims
		#:with-adb
		#:logout-user
		#:check-credentials)
  (:import-from :maxclaims/log/user-log
		#:with-log-database)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler
		#:redirect))
(in-package :maxclaims/entry-point/login)

(define-easy-handler (logout :uri "/ecm/logout")()
  (logout-user)
  (<:html
   (<:body
    (<:script
     "function deleteLogin() {
        d = new Date() ;
        document.cookie = 'ecm-login=; expires=' + d.toUTCString() + '; path=/;'
      };
      deleteLogin(); document.location = 'https://maxwellclaims.net'; "))))

(defun login-page (username password query)
  (<:as-is 
   (yasexml:<> (max-ecm/html/page:page)
    (yasexml:<> (max-ecm/html/login:login :username username 
					  :password password
					  :query query)))))

(define-easy-handler (login :uri "/ecm/login") 
    ((username :real-name "u")
     (password :real-name "p")
     (query :real-name "q"))
  (format *trace-output* "Hanfle Login in CL\n")
  (if (and username password 
	         (check-credentials username password))
      (redirect (or query "/ecm/index"))
      (login-page username password query)))
