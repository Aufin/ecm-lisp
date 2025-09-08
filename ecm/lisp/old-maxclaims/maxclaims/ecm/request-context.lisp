(cl:defpackage :ecm/request-context
  (:use :cl)
  (:import-from :ecm/json
		#:write-json-to-string)
  (:import-from :flexi-streams
		#:make-external-format)
  (:import-from :ecm/user)
  (:import-from :ecm/quri)
  (:export
   #:*json-mime-type*
   #:*utf-8*

   #:send-request-response
   #:send-json-response
   #:call-with-request-context		;
   #:with-request-context))
(cl:in-package :ecm/request-context)

(defvar *json-mime-type* "text/plain; charset=utf-8")

(defvar *utf-8* (make-external-format :utf-8 :eol-style :lf))

(defun send-request-response (mime-type body &key return-code external-format)
  (setf (hunchentoot:content-type*) mime-type)
  (when return-code
    (setf (hunchentoot:return-code*) return-code))
  (when external-format
    (setf (hunchentoot:reply-external-format*) external-format))
  (throw 'exit-call-with-request-context body))

(defun send-json-response (json &key return-code raw)
  ;; (warn "JASON: ~A ~W" json json)
  (send-request-response *json-mime-type* (if raw json (write-json-to-string json))
			 :return-code return-code
			 :external-format *utf-8*))

(defun handle-request-error (condition)
  (flet ((json ()
	   (send-json-response (ecm/json:jso "error" (format nil "~A" condition)
					     "backtrace" (with-output-to-string (s)
							   (trivial-backtrace:print-backtrace-to-stream s)))
			       :return-code 400)))
    (typecase condition
      (ecm/user:no-user-available
       (let* ((name (hunchentoot:script-name*))
	      (parameters (hunchentoot:get-parameters*))
	      (uri (ecm/quri:make-uri :path name
				      :query parameters))
	      (login-uri (ecm/quri:make-uri :path "/ecm/login" :query `(("q" . ,(ecm/quri:render-uri uri nil))))))
	 (if (string-equal (hunchentoot:content-type*) "text/html")
	     (ecm/hunchentoot:redirect (ecm/quri:render-uri login-uri))
	     (json))))
      (t (json)))))

(defun call-with-request-context (thunk)
  (catch 'exit-call-with-request-context
    (handler-bind
	((error #'handle-request-error))
      (ecm/user:with-user ()
	      (funcall thunk)))))

(defmacro with-request-context (nil &body body)
  (let ((thunk-name (gensym "WITH-REQUEST-CONTEXT-THUNK-")))
    `(flet ((,thunk-name ()
	      ,@body))
       (declare (dynamic-extent #',thunk-name))
       (call-with-request-context #',thunk-name))))
