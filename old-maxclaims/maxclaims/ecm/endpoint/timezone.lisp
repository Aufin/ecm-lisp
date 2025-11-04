(defpackage :ecm/endpoint/timezone
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/request-context)
  
  
  (:import-from :ecm/user)
  (:import-from :ecm/hunchentoot)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/json #:getjso))
(in-package :ecm/endpoint/timezone)

(define-endpoint timezone "ecm/timezone")

(defun <timezone-page> ()
;  (break "h:~A" (hunchentoot:session-value 'ecm/user::timezone))
  (multiple-value-bind (tz tz? session?)
      (ecm/user:user-timezone)
    (declare (ignore tz?))
 ;   (break "session~A ~%h:~A" session? (hunchentoot:session-value 'ecm/user::timezone))
    (if (not session?)
	(<> (ecm/ui/page:page :title "timezone")
	  (<> 'html5:script '#:|
$.ajax({
        type: "POST",
        url: "/ecm/timezone",
        contentType: "application/json; charset=utf-8",
        dataType: "json",
	data: JSON.stringify(
			     { location: moment.tz.guess(),
			       timezone: moment.tz(moment.tz.guess()).format('z'),
			       gmt_offset: moment.tz(moment.tz.guess()).format('Z')
			     }),
        success: function(data){
            location.reload();},
        failure: function(errMsg) {
            alert(errMsg);
        }
  });|))
	(<> (ecm/ui/page:page)
	  (<> :text (getjso "location" tz) " " (getjso "timezone" tz) " " (getjso "gmt_offset" tz))))))

(defun timezone/get ()
  (<timezone-page>))

(defun timezone/post ()
  (ecm/request-context:with-request-context ()
    (let ((tz (ecm/hunchentoot:get-post-body-as-json)))
      (setf (ecm/user:user-timezone) tz)
     ; (break "~A" (multiple-value-list (ecm/user:user-timezone)))
      (ecm/request-context:send-json-response 
       tz ))))




