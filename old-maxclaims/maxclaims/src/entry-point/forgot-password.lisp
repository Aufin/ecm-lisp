(defpackage :maxclaims/entry-point/forgot-password
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

(in-package :maxclaims/entry-point/forgot-password)

(defun forgot-password-page (username)
  (declare (ignore username))
  (<:as-is 
   (yasexml:<> (max-ecm/html/page:page)
    (yasexml:<> (max-ecm/html/forgot-password:forgot-password)))))

(define-easy-handler (forgot-password :uri "/ecm/forgot-password") 
    ((username :real-name "u")
     (reason :real-name "reason"))
  
  (if (and username reason)
      (progn (max-ecm/mail:send-forgot-password-mail 
	      username reason)
	     (<:as-is 
	      (yasexml:<> (max-ecm/html/page:page)
		(yasexml:<> (:unescaped 
			     '|
<div class="container">
    <div class="row">
        <div class="col-sm-6 col-md-4 col-md-offset-4">
            <h1 class="text-center login-title">Forgot Password?</h1>

            <div class="account-wall">|))
  
		(yasexml:<> (div :class "profile-img glyphicon glyphicon-remove-circle"
				 :style "font-size:5em; color:red; text-align:center"
				 )
		  (yasexml:<> (:text "")))
		(yasexml:<> (:text (concatenate 
				    'string "Request for Password has been sent.
                              Username :" username
				    "


" reason)))
  
		(yasexml:<> (:unescaped '|
           
            </div>
        </div>
    </div>
</div>|))
		(yasexml:<> style (yasexml:<> (:unescaped '|

.form-signin
{
    max-width: 330px;
    padding: 15px;
    margin: 0 auto;
}
.form-signin .form-signin-heading, .form-signin .checkbox
{
    margin-bottom: 10px;
}
.form-signin .checkbox
{
    font-weight: normal;
}
.form-signin .form-control
{
    position: relative;
    font-size: 16px;
    height: auto;
    padding: 10px;			;
    -webkit-box-sizing: border-box;
    -moz-box-sizing: border-box;
    box-sizing: border-box;
}
.form-signin .form-control:focus
{
    z-index: 2;
}
.form-signin input[type="text"]
{
    margin-bottom: -1px;
    border-bottom-left-radius: 0;
    border-bottom-right-radius: 0;
}
.form-signin input[type="password"]
{
    margin-bottom: 10px;
    border-top-left-radius: 0;
    border-top-right-radius: 0;
}
.account-wall
{
    margin-top: 20px;
    padding: 40px 0px 20px 0px;
    background-color: #f7f7f7;
    -moz-box-shadow: 0px 2px 2px rgba(0, 0, 0, 0.3);
    -webkit-box-shadow: 0px 2px 2px rgba(0, 0, 0, 0.3);
    box-shadow: 0px 2px 2px rgba(0, 0, 0, 0.3);
}
.login-title
{
    color: #555;
    font-size: 18px;
    font-weight: 400;
    display: block;
}
.profile-img
{

   color:red;
    width: 96px;
    height: 96px;
    margin: 0 auto 10px;
    display: block;
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
}
.need-help
{
    margin-top: 10px;
}
.new-account
{
    display: block;
    margin-top: 10px;
}|))))))
      (forgot-password-page username)))
