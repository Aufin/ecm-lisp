(defpackage :ecm/postgresql/connection
  (:use :cl)
  (:import-from :postmodern
                #:*database*
                #:connected-p
                #:disconnect
                #:reconnect
                #:disconnect-toplevel)
  (:import-from :ecm/postgresql/authentication)
  (:export #:*database* #:connect #:connected-p #:connect-toplevel
           #:disconnect
           #:reconnect #:disconnect-toplevel
           #:call-with-connection
           #:with-connection))
(in-package :ecm/postgresql/connection)

(defun normalize-connection-spec (spec)
  (destructuring-bind (database user password host &key (port 5432) pooled-p &allow-other-keys)
      spec
    (declare (ignore password))
    (list database user nil host :use-ssl :no :port port :pooled-p pooled-p)))


(defun %connect (fn spec)
  (destructuring-bind (database user password host &key (conf.d nil c-p)
                                                     (user.d nil u-p) &allow-other-keys)
      spec
    (declare (ignore password user database host))
    (apply #'ecm/postgresql/authentication:call-with-dynamic-postgresql-authentication-keys
           (second spec)
           (lambda ()
             (apply fn (normalize-connection-spec spec)))
           (nconc (when c-p (list :conf.d conf.d))
                  (when u-p (list :user.d user.d))))))

(defun connect (&rest connection-spec)
  (%connect #'pomo:connect connection-spec))

(defun connect-toplevel (&rest spec)
  (%connect #'pomo:connect-toplevel spec))

(defun call-with-connection (spec thunk)
  "Binds *database* to a new connection, as specified by the spec
argument, which should be a list of call-arguments-limit that can be passed to
connect, and runs the function given as a second argument with that
database."
  (flet ((cwc ()
           (let ((*database* (apply #'connect spec)))
             (unwind-protect (funcall thunk)
               (disconnect *database*)))))
    (let ((err nil))
      (handler-case
          (cwc)
        ((or cl-postgres-error:server-shutdown cl-postgres-error:admin-shutdown)
          (c)
         (warn "Ok, got this where?")
          (if err
              (error c)
              (progn (pomo:clear-connection-pool)
                     (setf err t)
                     (cwc))))))))

(defmacro with-connection (spec &body body)
  "Locally establish a database connection, and bind *database* to it."
  `(call-with-connection ,spec (lambda () ,@body)))
