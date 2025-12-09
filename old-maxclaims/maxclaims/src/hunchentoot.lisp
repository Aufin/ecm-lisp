(cl:defpackage :maxclaims/hunchentoot
  (:use :cl)
  (:import-from :hunchentoot
		#:query-string*
		#:redirect
		#:session-value
		#:session-too-old-p)
  (:import-from :ecm/hunchentoot
		#:ecm-acceptor)
  (:export #:define-easy-handler
	   #:query-string*
	   #:redirect
	   #:session-value
	   #:*hunchentoot-acceptor*
       #:make-maxclaims-acceptor))
(uiop/package:define-package :maxclaims-src/src/hunchentoot
	(:USE-REEXPORT :maxclaims/hunchentoot))

(in-package :maxclaims/hunchentoot)

(defun hunchentoot:session-too-old-p (session)
  "Returns true if the SESSION object SESSION has not been active in
the last \(SESSION-MAX-TIME SESSION) seconds."
  (and (not (hunchentoot:session-value :app-user session))
       (< (+ (hunchentoot::session-last-click session)
	     (hunchentoot:session-max-time session))
	  (get-universal-time))))

(defparameter hunchentoot:*session-max-time*
  (* 10 24 2 1800))


(defun hunchentoot:ssl-p (&optional _) (declare (ignore _)) t)

(defclass maxclaims-acceptor (ecm-acceptor hunchentoot:easy-acceptor)
  ())

(defun make-maxclaims-acceptor ()
  (make-instance 'maxclaims-acceptor
		 :port 4242
		 :message-log-destination
		 #P"/srv/ecm/var/log/ecm-lisp/message.log"
		 :access-log-destination
		 #P"/srv/ecm/var/log/ecm-lisp/access.log"
     :taskmaster
     (make-instance 'quux-hunchentoot:thread-pooling-taskmaster
                    :max-thread-count 1000
                    :max-accept-count 1024)))


(defvar  *hunchentoot-acceptor*
  (make-maxclaims-acceptor))

(defparameter *hunchentoot-static-handler*
  (hunchentoot:create-folder-dispatcher-and-handler
   "/static/" (merge-pathnames "static/"
			       (asdf:system-source-directory :maxclaims))))

(pushnew *hunchentoot-static-handler* hunchentoot::*dispatch-table*)

(defun start-server (&optional
                       (acceptor *hunchentoot-acceptor*)
                       (port (hunchentoot:acceptor-port acceptor)))
  (setf (slot-value acceptor 'hunchentoot::port) port)
  (handler-case (hunchentoot:start acceptor)
    ((or SB-BSD-SOCKETS:ADDRESS-IN-USE-ERROR USOCKET:ADDRESS-IN-USE-ERROR) ()
      (start-server acceptor (+ 1 port)))))

(defmacro define-easy-handler (description lambda-list
			       &body body)
  `(hunchentoot:define-easy-handler ,description ,lambda-list
     (yaclml:with-yaclml-output-to-string
       ,@body)))




