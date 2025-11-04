(uiop:define-package :ecm/hunchentoot
    (:use :cl :sexpml :hunchentoot)
  (:import-from :ecm/endpoint)
  (:import-from :ecm/json)
  (:reexport :hunchentoot)
  (:export #:ecm-acceptor
	   #:parameter-or-nil
	   #:get-post-body-as-json
	   #:*protocol*))
(in-package :ecm/hunchentoot)

(defparameter *protocol* :https)

(defclass ecm-request (hunchentoot:request) ())

(defclass ecm-session ()
  ((uuid :accessor ecm-session-uuid
	:initarg :uuid)))

;(hunchentoot:session-value 

#+(or)(defmethod hunchentoot:session-verify ((request ecm-request))
  (let ((session-identifier
	 (or (let ((session-cookie (cookie-in (session-cookie-name *acceptor*) request)))
	       (when session-cookie
		 (url-decode session-cookie)))
	     (get-parameter (session-cookie-name *acceptor*) request))))
    (when (and (stringp session-identifier)
               (ppcre:scan "^\\d+:.+" session-identifier))
      (destructuring-bind (id-string session-string)
          (ppcre:split ":" session-identifier :limit 2)
        (let* ((id (parse-integer id-string))
               (session (get-stored-session id))
               (user-agent (user-agent request))
               (remote-addr (remote-addr request)))
          (cond
            ((and session
                  (string= session-string
                           (session-string session))
                  (string= session-string
                           (encode-session-string id
						  
                                                  user-agent
                                                  (real-remote-addr request)
                                                  (session-start session))))
             ;; the session key presented by the client is valid
             (setf (slot-value session 'last-click) (get-universal-time))
             session)
            (session
             ;; the session ID pointed to an existing session, but the
             ;; session string did not match the expected session string
             (log-message* :warning
                           "Fake session identifier '~A' (User-Agent: '~A', IP: '~A')"
                           session-identifier user-agent remote-addr)
             ;; remove the session to make sure that it can't be used
             ;; again; the original legitimate user will be required to
             ;; log in again
             (remove-session session)
             nil)
            (t
             ;; no session was found under the ID given, presumably
             ;; because it has expired.
             (log-message* :info
                           "No session for session identifier '~A' (User-Agent: '~A', IP: '~A')"e
                           session-identifier user-agent remote-addr)
             nil)))))))

(defclass ecm-acceptor () ()
  (:default-initargs :request-class 'ecm-request))

(defmethod hunchentoot:acceptor-dispatch-request :around
    ((acceptor ecm-acceptor) request)
  (handler-case (ecm/endpoint:endpoint-dispatcher
		 (hunchentoot:script-name request) 
		 (hunchentoot:request-method*))
    (ecm/endpoint:endpoint-not-found ()
      (call-next-method))))

(defun parameter-or-nil (name &key (identity #'identity)
				(parameter #'hunchentoot:parameter))
  (let ((value (funcall parameter name)))
    (when (and value (not (string= value "")))
      (funcall identity value))))

(defun get-post-body-as-json ()
  (let* ((raw-json (hunchentoot:raw-post-data))
	 (stringized-json
	  (if (stringp raw-json)
	      raw-json
	      (sb-ext:octets-to-string raw-json :external-format :utf-8))))
    (let ((*read-default-float-format* 'double-float))
      (ecm/json:read-json-from-string stringized-json))))           
                                       
    
    
    

