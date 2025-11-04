;; (in-package :maxclaims)

;; (defcomponent test ()())

;; (defmethod render ((test test))
;;   (<:as-html "Coming Soon."))

;; (defcomponent application-container (container)
;;   ())

;; (defmethod render ((app application-container))
;;   (render (find-component app (current-component $window))))

;; (defmethod shared-initialize :after ((app application-container) slots &rest args)
;;   (setf (find-component app 'home-component) (make-instance 'home-component))
;;   app)

;; (defparameter *nav-menu*
;;   '("Home"           home-component nil
;;     "Reports"         reports-component nil
;;     "Advanced Search" advanced-search-component :read-write
;;     "History"  history-component :read-write
;;     "Admin"    admin-component :admin))

;; (defun render-navigation-menu ()
;;   (<:ul
;;    :id "navigation-list"
;;    :style "list-style:none"
;;    (loop for (name component permissions) on *nav-menu* by #'cdddr
;;       :do (let ((component component))
;; 	    (when (or (not permissions)
;; 		      (and (eq :read-write permissions)
;; 			   (not (user-read-only-p $app-user)))
;; 		      (and (eq :admin permissions)
;; 			   (app-user.admin $app-user)))
;; 	      (<:li (<ucw:a
;; 		     :action (progn (call-component $body (make-instance component))
;; 				    (app-user-log "called" name "from nav menu"))
;; 		     (<:as-html name))))))
;;    (<:li (<ucw:a
;; 	  :action (logout)
;; 	  (<:as-html "Logout"))))
;;       (<:div
;;      :style "float:left"
;;      (<:form
;;       :style "display:inline"
;;       :action "/maxclaims/ecm/search"
;;       (<:input :name "q")
;;       (<:submit))))

;; (defcomponent maxclaims-window (standard-window-component)
;;   ((current-component :initarg :component
;; 		      :component home-component :accessor current-component))
;;   (:default-initargs
;;       :title (format nil "Maxwell Claims ECM: ~A "
;; 		     (asdf:component-version (asdf:find-system :maxclaims)))
;;     :stylesheet '(
;;     ;		  "http://yui.yahooapis.com/2.6.0/build/grids/grids-min.css"
;;     ;	          "http://yui.yahooapis.com/2.6.0/build/base/base-min.css"
;; 		  "/maxwell/stylesheet-new.css")
;;     :javascript `((:src "http://yui.yahooapis.com/combo?2.8.2r1/build/yuiloader-dom-event/yuiloader-dom-event.js")
;; 		  (:script
;; 		   ,(js:js
;; 		     (js:defvar *maxclaims-yui-loader*
;; 		       (js:new
;; 			(*YAHOO*.util.|:YUILoader|
;; 				      (js:create
;; 				       :on-success
;; 				       (lambda ()
;; 					 (dolist (callback this.after-load-callbacks)
;; 					   (callback))
;; 					 (setf this.after-load-callbacks nil))))))
;; 		     (setf *maxclaims-yui-loader*.really-insert
;; 			   (lambda (required callback)
;; 			     (dolist (req required)
;; 			       (this.require req))
;; 			     ;; fixme: overloading the meaning of
;; 			     ;; a-l-c... before the window has
;; 			     ;; finished loading we want to call the
;; 			     ;; callbacks then, but once that happens
;; 			     ;; we set the list to null and want to
;; 			     ;; execute callbacks immediately
;; 			     (if this.after-load-callbacks
;; 				 (this.after-load-callbacks.push callback)
;; 				 (callback))))
;; 		     (setf *maxclaims-yui-loader*.after-load-callbacks (array))
;; 		     (*yahoo*.util.*event.on-d-o-m-ready
;; 		      (lambda () (*maxclaims-yui-loader*.insert))))))
;;     :body (make-instance 'maxclaims-login-component)))

;; (defmethod render-html-body  ((self maxclaims-window))
;;  ;; (break "~A" *db-connection-parameters*)
;;   ;;(break "~A ~A" $app-user (context.session *context*))
;;   (let* ((app-user $app-user)
;; 	 (*db-connection-parameters*
;; 	  (list (first *db-connection-parameters*)
;; 		(if app-user
;; 		    (app-user-rolename app-user)
;; 		    "mrl_lookup_user")
;; 		(if app-user
;; 		    (app-user.password app-user)
;; 		    "qu0hhu@n")
;; 		(car (last *db-connection-parameters*)))))
;;     (with-db
;;       (<:div
;;        :id "doc3"
;;        (<:style" tr.lol-attribute td {border-bottom:2px solid #eeeeee; padding-right:1em;}")
;;        (if app-user
;; 	   (progn
;; 	     (<:div
;; 	      :id "header"
;; 	      (<:as-html (ucw::cookies (ucw::context.request ucw::*context*)))
;; 	      (with-active-descriptions (inline)
;; 		(display self $app-user :attributes '((username :label nil)))
;; 		(<:as-html " logged in."))
;; 	      (render-navigation-menu))
;; 	     (<:div
;; 	      :style "clear:both; margin-left:5%;margin-right:5%;"
;; 	      (render (current-component self))))
;; 	   (call-next-method))))))
