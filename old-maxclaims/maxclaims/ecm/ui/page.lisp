(uiop:define-package :ecm/ui/page
    (:use :cl :sexpml)
  (:shadowing-import-from #:ecm/ml #:<>)
  (:import-from :ecm/ui/bootstrap)
  (:import-from :ecm/ui/jquery)
  (:import-from :ecm/ui/pdfjs)
  (:import-from :ecm/endpoint/pdf)
  (:import-from :ecm/endpoint/webodf)
  (:import-from :ecm/ui/font-awesome)
  (:import-from :ecm/ui/tether)
  (:import-from :ecm/ui/moment)
  (:import-from :ecm/ui/datatables)
  (:import-from :ecm/ui/dropzone)
  (:import-from :ecm/user
		#:with-user
		#:no-user-available)
  (:export #:page))
(in-package :ecm/ui/page)

;; http://v4-alpha.getbootstrap.com/getting-started/introduction/#important-globals

(sexpml:define-tag page
  (sexpml:sexpml-attributes-bind ((title nil)
				  (user t)
				  (refresh nil)
				  (ecm-body-class "container")
				  (bootstrap t)
				  (font-awesome t)
				  (datatables nil)
				  (dropzone nil)
				  (webodf nil))
      (sexpml:tag-attributes)
    `(block foo
       (handler-bind
	   ((no-user-available
	     (lambda (c)
	       (declare (ignore c))
	       (return-from foo
		 (hunchentoot:redirect
				   
		  (format nil "/ecm/login?q=~A"
			  (drakma:url-encode (hunchentoot:request-uri*) :ascii)))))))
	 (with-output-to-string (*sexpml-output*)	   
	   ,(page
	     :title title :user user :class ecm-body-class
	     :refresh refresh :contents (sexpml:tag-contents)
	     :font-awesome font-awesome :datatables datatables
	     :webodf webodf
	     :dropzone dropzone
	     :bootstrap bootstrap))))))
  

(defun page (&key 
	             title user refresh contents (class "container")
	             (font-awesome t)
	             (datatables nil)
	             (dropzone nil)
	             (moment t)
	             (summernote nil)
	             (webodf nil)
	             (bootstrap t))
  
  `(<> (html :doctype "html")
     (<> (html5:head)
       (<> :unescaped
	       '#:|<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">|)

       ,(when refresh 
	        `(<> (meta :http-equiv "refresh"
		                 :content ,refresh)))
       ,(when title
	        `(<> (html5:title) (<> :text ,title)))
              
       ,(when font-awesome
	        `(<> 'ecm/ui/font-awesome:css))
       
       ,(when webodf
	        (sexpml:sexpml-form
	         'html5:link
	         :attributes '(:href "/ecm/webodf/demo-viewer.css"
			                   :rel "stylesheet"
			                   :crossorigin "anonymous")))
       
       ,(when bootstrap
	        '(<> 'ecm/ui/bootstrap:css))
       (<> 'ecm/ui/jquery:css)

       ,(when datatables
	        `(<> (ecm/ui/datatables:css)))

       ,(when dropzone
	        `(<> (ecm/ui/dropzone:css)))
       (<> 'ecm/ui/jquery:js)
       (<> 'ecm/ui/tether:js)
       (<> 'ecm/ui/bootstrap:js)
       ,(when dropzone
	        `(<> 'ecm/ui/dropzone:js))
       ,(when webodf
	        '(progn
	          (<> (script :src "/ecm/webodf/webodf.js"))
	          (<> (script :src "/ecm/webodf/demo-viewer.js"))))
       ,(when moment '(<> 'ecm/ui/moment:js))
                                        ;       ,(when pdfobject '(<> 'ecm/ui/pdfobject:js))
       ,(when datatables
	        '(<> (ecm/ui/datatables:js)))

       ,(when summernote
	        '(<> :unescaped
	          '|<!-- include summernote css/js-->
<link href="http://cdnjs.cloudflare.com/ajax/libs/summernote/0.8.1/summernote.css" rel="stylesheet">
<script src="http://cdnjs.cloudflare.com/ajax/libs/summernote/0.8.1/summernote.js"></script>|)))

     (<> (html5:body :class "collapse-hover")
       (<> 'script "if ((\"ontouchstart\" in document.documentElement)) {
document.documentElement.className += \" can-touch\";
}")
       (<> (div :class ,class :id "ecmBody")
	       ,@(if user
	             `((with-user () ,@contents))
	             contents)))))
