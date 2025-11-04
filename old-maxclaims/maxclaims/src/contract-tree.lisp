(in-package :maxclaims)

(defcomponent contract-tree ()
  ((contracts :accessor contracts :initarg :contracts)))

(defclass dojo-tree-action  (ucw::standard-action)
  ()
  (:metaclass mopp:funcallable-standard-class))

;;TODO: integrate into lol's display-using-description protocol.
(defun test-ajax-render (d o c)
  (with-output-to-string (s)
    (let ((attributes (lol::find-do-attributes d)))
      (mapcar (lambda (a)
		(let ((attribute (find-attribute d a)))		  
		  (when attribute 
		    (format s
			    (escape-as-html
			     (format nil "| ~A : ~A |"
				     (lol::label attribute)
				     (attribute-value o attribute)))))))
	      attributes))))

(lol::define-layered-method display
    :in-layer dojo-tree-layer
    ((component dojo-tree-action) object &rest args)
    (funcall (lol::make-display-function component object)
	     'test-ajax-render))

(defun find-expanded-object (frame)
  (funcall
   (ucw::find-action
    frame
    (cdr (assoc
	  :object-id
	  (cdr  (assoc
		 :node
		 (json:decode-json-from-string
		  (get-parameter
		   (ucw::context.request *context*)
		   "data")))))))))

(defmethod tree-folder-p (object)
  t)

(defmethod tree-folder-p ((object claimtransaction))
  nil)

(defun expand-tree-for-object (action frame object function)
  (mapcar 
   (lambda (new-object)
     (json:encode-json-alist-to-string
      `((title . ,(display action new-object))
	(is-folder . ,(tree-folder-p new-object))
	(object-id . ,(ucw::register-action-in-frame
		       frame
		       (make-action  (lambda ()
				       new-object)))))))
   (funcall function object)))

(defmethod expand-tree (action frame (object contract))
  (expand-tree-for-object action frame object 'find-policies))

(defmethod expand-tree (action frame (object policy))
  (expand-tree-for-object action frame object 'policy-claims))

(defmethod expand-tree (action frame (object claim))
  (expand-tree-for-object action frame object 'claim-claimtransactions))

(defmethod ucw::handle-action ((action dojo-tree-action) app sess frame)
  (lol::with-active-layers (dojo-tree-layer)   
    (ucw::funcall-with-request-context
     *context*
     (lambda ()
       (let ((object (find-expanded-object frame)))

	 (format (ucw::html-stream (ucw::context.response *context*))
		 "[~{~A~^,~}]"
		 (expand-tree action frame object))))
     :content-type "text/javascript"))
  t)


(defclass dojo-tree-select-action (ucw::standard-action)
  ()
  (:metaclass mopp:funcallable-standard-class))


(defmethod ucw::handle-action ((action dojo-tree-select-action) app sess frame)
  (ucw::funcall-with-request-context
   *context*
   (lambda ()
     (setf (context.session *context*) sess)
     (let ((object (funcall
		    (ucw::find-action
		     frame
		     (get-parameter (ucw::context.request *context*)
				    "_object")))))
       (setf (instance (display-of (body (ucw::frame.window-component frame)))) object))
     (yaclml::with-yaclml-stream (ucw::html-stream (ucw::context.response *context*))
       (render (display-of (body (ucw::frame.window-component frame)))))))
  t)


(defun make-tree (self)
  (<ucw:script

   `(defun $ (e)
      (return  (document.get-element-by-id e)))

   `(defun tree-select-fired ()
      (alert "foo"))

   `(defun build-tree-nodes (data-objects tree-parent-node)
      (dotimes  (i data-objects.length)
	(let ((node (dojo.widget.create-widget
		     "TreeNode"
		     (aref data-objects i))))
			  
	  (tree-parent-node.add-child node)
	  (tree-parent-node.register-child node i)
	  (when (slot-value (aref data-objects i ) 'children)
	    (build-tree-nodes (slot-value (aref data-objects i) 'children) node)))))

   `(defun build-tree (name nodes container placeholder url)
      (let (widget
	    (selector
	     (dojo.widget.create-widget
	      "TreeSelector"
	      (create :widget-id "tree-selector")))
	    (controller
	     (dojo.widget.create-widget
	      "TreeRPCController"
	      (create :widget-id "tree-controller"
		      :*R-P-C-url url
		      :*D-n-d-controller "create"))))

	(container.append-child selector.dom-node)
	(container.append-child controller.dom-node)
	
	(setf widget (dojo.widget.create-widget
		      "Tree"
		      (create :widget-id name
			      :selector "tree-selector"
			      :controller "tree-controller")))
	
	(build-tree-nodes nodes widget)

	(dojo.event.connect
	 selector "select"
	 (lambda (val)
	   (dojo.io.bind
	    (create
	     :url
	     (+ ,(strcat
		  (action-href
		   (make-action
		    (lambda ()
		      ())
		    :class 'dojo-tree-select-action))
		  "&%5fobject=")
		selector.selected-node.object-id)
	     :load
	     (lambda (type data event)
	       (setf (slot-value
		      ($ "display-of")
		      'inner-h-t-m-l)
		     data))))))

	(container.replace-child widget.dom-node placeholder)))

   `(dojo.add-on-load
     (lambda ()
       (build-tree "my-new-tree"
		   tree-data
		   ($ "tree-container")
		   ($ "tree-placeholder")
		   ,
		   (action-href
		    (make-action (lambda () ) :class 'dojo-tree-action)))))))

(defmethod render
    ((self contract-tree))
  (<:div
   :style "height:50%,border:1px solid black"
	 
   (<:div
    :id "tree-container"
    (<:span
     :id "tree-placeholder"
     (<:as-html "Loading Tree ...."))))

  (<:script
   (<:as-is
    (format nil "var treeData = [~{~A~^,~}];"
	    (mapcar (lambda (c)
		      (json:encode-json-alist-to-string
		       `((:is-folder . true)
			 (:object-id . ,(ucw::register-action-in-frame
					 (ucw::context.current-frame *context*)
					 (make-action  (lambda ()
							 c))))
			 (:title . 
				 ,(display self c
					   :attributes '(contract-number contract-year)
					   :layers '(+ as-string
						     - show-attribute-labels))))))
		    (list (first (contracts self)))))))

  (make-tree self))

#|     (dolist (c '())
     (<:div
      (@ "dojoType" "TreeNode"
		    "isFolder" "true"
		    "title" )
      
      #+nil(dolist (name '(contract-commercials
			   contract-travels
			   contract-marines
			   contract-snowmobiles
			   contract-automobiles))
	     (let* ((attribute (find-attribute (find-occurence c) name))
		    (value (slot-value c name)))
	       (if value 
		   (<:div
		    (@ "dojoType" "TreeNode"
				  "isFolder" "true"
				  "title" (format nil "~A (~A)"
						  (lol::label attribute)
						  (length value)))
		    (dolist (v value)
		      (<:div
		       (@ "dojoType" "TreeNode"
				     "isFolder" "true"
				     "title" (display self v
						      :layers '(+ as-string)))))))))))
  ()
  (<:div
   (@ "dojoType" "Tree")
)) |#