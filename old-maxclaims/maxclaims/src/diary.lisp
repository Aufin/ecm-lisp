(in-package #:maxclaims)

(defun universal-time->simple-date (&optional (ut (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day daylight-p)
      (decode-universal-time ut)
    (declare (ignorable second minute hour day daylight-p))
    (simple-date:encode-date year month date)))

(defun diary-for-user-p (user)
  (car (select-limit* 1 0 
		      `(:select diary-entry-id 
			:from 'diary-entry 
			:where (:= app_user_id ,(app-user.app-user-id user))))))

(defun diary-outstanding-p (user)
  (select-limit* 
   1 0 
   `(:select diary-entry-id 
     :from 'diary-entry 
     :where (:and (:= app_user_id ,(app-user.app-user-id user))
		  (:<= action-date (:now))
		  (:not 'processed)
		  (:not (:exists 
			 (:select 'defer-date 
			  :from (:as defer-diary-entry d)
			  :where (:and (:= d.diary-entry-id 
					   diary-entry.diary-entry-id)
				       (:> 'defer-date (:now))))))))))
		 

(defun partition-diary-entries (diary-entries split-offsets)
  "Return subsets of DIARY-ENTRIES split at offsets from today"
  (let*  ((today (universal-time->simple-date))
	  (boundaries (mapcar (lambda (offset)
				(simple-date:time-add
				 today
				 (simple-date:encode-interval :day offset)))
			    split-offsets)))

    (loop for b in boundaries 
	  collect 
	  (let ((pos (position-if (lambda (e)
				    (simple-date:time> (diary-entry.action-date e)  b))
				  diary-entries)))
	    (prog1 (subseq diary-entries 0 pos)
	      (setf diary-entries
		    (and pos (nthcdr pos diary-entries))))))))

;;; fixme: no way to fetch processed entries
(defun find-diary-entries (start-date days user)
  (query-objects 
   'diary-entry
   (lambda (table fields)
     `(:order-by (:select ,@fields :from ,table
		  :where (:and (:not 'processed)
			       (:not (:exists 
				      (:select 'defer-date 
				       :from (:as defer-diary-entry d)
				       :where (:and (:= d.diary-entry-id 
							diary-entry.diary-entry-id)
						    (:> 'defer-date (:now))))))
			       ,@(when start-date `((:>= 'action-date ,start-date)))
			       ,@(when days `((:<= 'action-date
						   ,(simple-date:time-add
						     (universal-time->simple-date)
						     (simple-date:encode-interval :day days)))))						     
			       ,@(when user `((:= 'app-user-id
						  ,(app-user.app-user-id user))))))
		 'action-date))))

(defcomponent diary-viewer ()
  ;; nil -> forever
  ((max-days :initarg :limit :initform nil :accessor max-days)
   (user :initarg :user :accessor user)
   ;; nil = -infinity
   (start :initarg :start :initform nil :accessor start-date)
   (boundaries :initarg :boundaries :initform '((nil . "All"))
	       :accessor boundaries ; todo: might want a higher level interface
	       :documentation "((boundary . label) ...), entries will
	       be split at BOUNDARY and labelled LABEL.")))

(defcomponent maxclaims-diary-window (standard-window-component)
  ((diary-entries :initarg :component 
		      :component (diary-viewer
					    :boundaries '((-1 . "OVERDUE")
					    (0 . "Today")
					    (7 . "Next 7 Days")
					    (14 . "Next 14 Days"))
			      :limit 30)
		      :accessor current-component)
   (diary-days :initform 14))
  (:default-initargs
      :title (format nil "MaxClaims Diary ~A : Maxwell Claims Inc"
		     (asdf:component-version (asdf:find-system :maxclaims)))
    :stylesheet '(
    ;		  "http://yui.yahooapis.com/2.6.0/build/grids/grids-min.css"
    ;	          "http://yui.yahooapis.com/2.6.0/build/base/base-min.css"
		  "/maxwell/stylesheet-new.css")
    :javascript `((:src "http://yui.yahooapis.com/combo?2.8.2r1/build/yuiloader-dom-event/yuiloader-dom-event.js")
		  (:script
		   ,(js:js
		     (js:defvar *maxclaims-yui-loader*
		       (js:new
			(*YAHOO*.util.|:YUILoader|
				      (js:create
				       :on-success
				       (lambda ()
					 (dolist (callback this.after-load-callbacks)
					   (callback))
					 (setf this.after-load-callbacks nil))))))
		     (setf *maxclaims-yui-loader*.really-insert
			   (lambda (required callback)
			     (dolist (req required)
			       (this.require req))
			     ;; fixme: overloading the meaning of
			     ;; a-l-c... before the window has
			     ;; finished loading we want to call the
			     ;; callbacks then, but once that happens
			     ;; we set the list to null and want to
			     ;; execute callbacks immediately
			     (if this.after-load-callbacks
				 (this.after-load-callbacks.push callback)
				 (callback))))
		     (setf *maxclaims-yui-loader*.after-load-callbacks (array))
		     (*yahoo*.util.*event.on-d-o-m-ready
		      (lambda () (*maxclaims-yui-loader*.insert))))))
      :body (make-instance 'diary-viewer)))


(defmethod render ((self diary-viewer))
  (with-slots ((days max-days) start boundaries)
      self
    (let ((user (if (slot-boundp self 'user)
		    (user self)
		    (setf (user self) $app-user)))) ; EVIL
      (render-diary-entries user start days
			    (mapcar #'car boundaries)
			    (mapcar #'cdr boundaries)))))

(defmethod render-home ((v diary-viewer) self)
   ;; fixme: this is a (possible) work-around... for some reason
     ;; referencing a session value in the :initform for a
     ;; component/backtracked slot is NOT grabbing the correct value
     ;; despite the user being set before instantiating
     ;; HOME-COMPONENT... might be a ucw-core bug, need to investigate
     ;; further
     (when (not (slot-boundp (slot-value self 'diary-entries) 'user))
	 (setf (user (slot-value self 'diary-entries)) $app-user))
     
     
     (<:h1 :style "clear: right" "Diary")
     ;; todo: diary-navigator component
     (<ucw:form :action (set-home-diary-days! (slot-value self 'diary-entries)
					      (slot-value self 'diary-days))
		(<:table (<:tr
			  (<:td (<:submit :value "View"))
			  (<:td (<ucw:select 
				 :accessor (slot-value self 'diary-days)
				 (<ucw:option 
				  :value 0
				  (<:as-html "Today"))
				 (dolist* (day '(30 60 90 180))
				   (<ucw:option 
				    :value day
				    (<:format "~D Month~:P" (/ day 30))))
				 (<ucw:option :value nil (<:ah "All"))))
			  (when (app-user.admin $app-user)
			    (<:td (<ucw:select :accessor (user (slot-value self 'diary-entries))
				    (<ucw:option :value nil "All users")
				    (<ucw:option :value $app-user (<:ah (app-user.username $app-user)))))))))
     (render v))



(defun render-diary-entries (user start days split label)
  (let* ((entries (find-diary-entries start days user))
	 (partitioned-entries (if (car split) ; nil = all entries
				  (partition-diary-entries entries split)
				  (list entries))))
    (when (notevery #'null partitioned-entries)
      (dolist* (entries (mapcar #'cons label  partitioned-entries))
	(when (cdr entries)
	  (let ((*source-component* $body)) ; FIXME: hack around being
					    ; unable to set links in a
					    ; subcomponent to take
					    ; over the parent
					    ; component in a better
					    ; place (`render-home'
					    ; ...)
	    (<:h2 (<:ah (car entries)))
	    (with-active-descriptions (inline list-as-table)
	      (<:table :style "width: 100%"
		       (<:thead
			(with-active-descriptions (description-as-table-header-2)
			  (display *current-component* (second entries))))
		       (<:tbody
			(with-active-descriptions (description-as-table-row)
			  (dolist* (entry (cdr entries))
			    (display *current-component* entry)))))))
	    
	  #+nil(with-active-descriptions (list-as-table)
		 (display *current-component* (cdr entries)))
	  #+nil(<:ul
		(dolist* (entry (cdr entries))
		  (<:li (<ucw:a :action (view-object entry) "View")
			" "
			(display-inline entry)))))))))


(defmethod render-html-body  ((self maxclaims-diary-window))
 ;; (break "~A" *db-connection-parameters*)
  ;;(break "~A ~A" $app-user (context.session *context*))  
  (let* ((app-user $app-user)
	 (*db-connection-parameters* 
	  (list (first *db-connection-parameters*)
		(if app-user 
		    (app-user-rolename app-user)
		    "mrl_lookup_user")
		(if app-user
		    (app-user.password app-user)
		    "qu0hhu@n")
		(car (last *db-connection-parameters*)))))
    (with-db 
      (<:div 
       :id "doc3"
       :style "margin:10px;"
       (<:style" tr.lol-attribute td {border-bottom:2px solid #eeeeee; padding-right:1em;}")
      (render-home (current-component self) self)))))
