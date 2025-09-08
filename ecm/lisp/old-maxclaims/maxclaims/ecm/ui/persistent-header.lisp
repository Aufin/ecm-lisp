(defpackage :ecm/ui/persistent-header
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/utility #:cat)
  (:export #:<persistent-header-script>))
(in-package :ecm/ui/persistent-header)


(defun <clone-and-persist-script> ()
  (<> (script)
    (ps:ps
      (defun set-cloned-tr-width (parent clone)
	(let ((target-children (parent.children)))
	  (|.|
	   clone (children)
	   (width (lambda (i val)
		    (|.| target-children (eq i) (width)))))))

      (defun remove-clones (&optional (collection ($ document )))
	(|.| ($"[data-clone]") (remove)))

      (defun find-clone (element)
	      (let ((id (|.| ($ element) (attr "id"))))
	        (|.| ($ (+"[data-clone="id"]")) )))

      (defun clone-and-persist (collection &key (initialize-clone
						                                     (lambda (c) c)))
	      (let ((clones #()))
	        (|.|
	         ($ collection)
	         (each
	          (lambda ()
	            (let* ((parent ($ this))
		                 (clone (parent.clone))
		                 (width (|.| parent (parent) (width)))
		                 (height (parent.height))
		                 (parent-id (progn
				                          (|.| parent (unique-id))
				                          (|.| parent (attr "id"))))
		                 (z-index (+ 1 (|.| parent (parent) (z-index)))))

		            (|.| clone (remove-attr "id"))

		            (clone.attr "data-clone" parent-id)
		            (|.| parent (z-index z-index))
		            (|.| clone (z-index (- z-index 1)))
		
		            (clone.css
		             ({} :position "absolute"
		                 :top (|.| parent (position) top)
		                 :left (|.| parent (position) left)))

		            (when width
		              (if (clone.is "tr")
		                  (set-cloned-tr-width parent clone)
		                  (progn
			                  (clone.width width)
			                  (clone.css
			                   ({} :width width)))))
		            #+(or)  (when height
			                    (clone.css
			                     ({} :height height)))
                (console.log parent)
                (unless (or ($. parent (is "tr"))
                            ($. parent (find ".ecm-nav-buttons") length))
                  ($. ".ecm-nav-buttons" (first) (clone)
                      (remove-class "d-md-inline-block")
                      (remove-class "d-none")
                      (prepend-to parent)
                      (find "a") (hide)))
		            (initialize-clone clone)
		            (clones.push clone)
		            (parent.after clone)))))

	        (ps:return-from clone-and-persist clones))))))

(defun <persistent-header-script> ()
  (<clone-and-persist-script>)
  (<> (script)
    (ps:ps
      (defun update-persistent-header (element &key (remove ps:false))
		  ;  (console.log "Update Persist:" element)
	      (let* ((pclass (|.| ($ element) (data "persistent-class")))
	             (header-selector (|.| ($ element) (data "persistent-header")))
	             (header (|.| ($ header-selector element) (first)))
	             (offset (|.| ($ element) (offset)))
	             (scroll-top (+ (|.| ($ window) (scroll-top))
			                        (|.| ($ ".ecm-nav") (height))))
	             (clone  (find-clone header)))

	        ;;(console.log clone.length)
	        (if (and (not remove) (> scroll-top (|.| offset top)))
	            (let* ((no-clone (eql 0 clone.length))
                     (clone (if no-clone
				                        (progn (clone-and-persist header)
				                               (find-clone header))
				                        clone))
		                 (width (|.| ($ clone) (width))))
		            (|.| ($ clone) (css "position" "relative"))
		            (when (header.is "tr")
		              (set-cloned-tr-width clone header)
		              (setf width (|.| ($ clone) (parent) (width))))
		            (|.| ($ header)
		                 (add-class pclass)
		                 (css "z-index" "10042")
		                 (width width)
		                 (attr "data-persistent-live" "true")))
	                (progn
		                (unless (eql 0 clone.length)
		                  (|.| ($ clone) (remove)))
		                (|.| ($ header) (remove-class pclass)
		                     (remove-attr "data-persistent-live"))))))

      (defun update-persistent-headers (collection &key (remove ps:false))
	      (|.|
	       ($ collection)
	       (each (lambda ()
		             (update-persistent-header this :remove remove)))))

      ;; At load time, this is run.

      ($ (lambda ()
         ;;  (alert "persist")
	         (let ((p ($".persist-area")))
	           (|.| ($ window )
		              (scroll (lambda ()
			                      (update-persistent-headers p)
			                      (|.| ($ "[data-persistent-live]")
				                         (css "margin-left" (+ "-" (|.|
							                                              ($ this)
							                                              (scroll-left))
						                                           "px")))))
		              ;(trigger "scroll")
		              (resize (lambda ()
			                      (update-persistent-headers p :remove t)
			                      (update-persistent-headers p)
			                      (|.| ($ "[data-persistent-live]")
				                         (css "margin-left" (+ "-" (|.|
							                                              ($ this)
							                                              (scroll-left))
						                                           "px")))))
                                        ;(trigger "resize")
                  )
	           (|.| ($ "a[data-toggle=\"tab\"]")
		              (on "shown.bs.tab"
		                  (lambda () (update-persistent-headers p))))))))))

