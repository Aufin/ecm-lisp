#+quicklisp (ql:quickload 
	     '("hunchentoot"
	       "cxml-stp"
	       "local-time"
	       "alexandria"
	       "split-sequence"))

(defpackage :pwap/simple-forum
  (:use :cl)
  (:import-from :pwap/yasexml
		#:<>
		#:wrap-in-tag)
  (:import-from :pwap/yasexml/unparse
		#:hax-unparse-handler)
  (:import-from :hunchentoot)
  (:import-from :alexandria)
  (:import-from :cl-fad)
  (:import-from :split-sequence))

(in-package :pwap/simple-forum)

;;; * Hunchentoot
(defvar *hunchentoot-acceptor*
  (make-instance 'hunchentoot:easy-acceptor :port 8686))

(defun start-server (&optional (acceptor *hunchentoot-acceptor*))
  (hunchentoot:start acceptor))

(defparameter *storage-pathname* 
  (merge-pathnames #P"pwap/forum/simple/" (user-homedir-pathname)))

(defun map-http-parameters (|function (name value)|)
  (loop :for (name . value) 
     :in (append (hunchentoot:get-parameters*)
		 (hunchentoot:post-parameters*))
     :do  (funcall |function (name value)| name value)))

(defun http-parameter-value (name 
			     &optional 
			       (parameter #'hunchentoot:parameter))
  (funcall parameter name))

(defun http-parameters-as-alist (&optional alist-name)
  (let (alist)
    (map-http-parameters
     (lambda (name value)
       (when (stringp value)
	 (if alist-name
	     (let ((alist-name? (subseq name 0 (min (length name) 
						    (length alist-name)))))
	       (when (string= alist-name alist-name?)
		 (let ((alist-key (subseq name (1+ (length alist-name))
					  (1- (length name)))))
	       
		   (push (cons alist-key value) alist))))			       
	     (push (cons name value) alist)))))
    (nreverse alist)))


;;; * XML 

(defmethod wrap-in-tag (fn (tag (eql 'stp-builder)) &rest _ &key (name "forum"))
  (declare (ignorable _))
  (<> (:handler (cxml-stp:make-builder))
    (<> (:document :name name)
      (funcall fn))))

(defmethod wrap-in-tag (fn (tag (eql 'xml-sink)) &rest _ &key (name "forum"))
  (declare (ignorable _))
  (<> (:handler (cxml:make-string-sink :indentation 1))
    (<> (:document :name name)
      (funcall fn))))


;;; * Web Display 

(defmethod wrap-in-tag (fn (tag (eql 'html-sink))  
			&key (title "Simple Forum | PWAP"))

  (<> (:handler (closure-html:make-string-sink) 
		:class 'pwap/yasexml/unparse:hax-unparse-handler)

    (<> (:document :name 'html)
      (<> html (<> head (<> title (<> (:text title))))
	  (<> body
	    (funcall fn))))))



(defun add-post (post-alist)
  (flet ((a* (name)
	   (cdr (assoc name post-alist
		       :test #'string=))))
    (ensure-directories-exist *storage-pathname*)
    (let ((post-as-string 
	   (<> (xml-sink :name "post")
	     (<> (post :author (a* "author")) 
	       (<> subject
		 (<> (:text (a* "subject"))))
	       (<> body
		 (<> (:text (a* "body")))))))
	  (post-pathname (merge-pathnames 
			  (make-pathname 
			   :name (concatenate 
				  'string  
				  (princ-to-string (get-universal-time)) 
				  "|" (a* "author") "|" (a* "subject")))
			  *storage-pathname*)))
      (alexandria:write-string-into-file post-as-string post-pathname
					 :if-does-not-exist :create
					 :external-format :utf8))))

(defun display-post-form ()
  (<> (form :method "POST" :action "#")
	    (<> "Author :")
	    (<> (input :type "text"
		       :name "post[author]"))
	    (<> br)
	    (<> "Subject :") 
	    (<> (input :type "text"
		       :name "post[subject]"))
	    (<> br)
	

	    (<> (textarea :rows 15 :cols 80
			  :name "post[body]"))
	    (<> br)
	    (<> (input :type "submit"))))

(defun display-post-list (list-of-post-pathnames)
  (<> ul
    (dolist (file list-of-post-pathnames)
      (destructuring-bind (time author subject)
	  (split-sequence:split-sequence #\| (pathname-name file))
	(let* ((date (local-time:format-timestring 
		      nil (local-time:universal-to-timestamp
			   (parse-integer time)))))
		
	  (<> li
	    (<> (a :href (format nil "view?post=~A" 
				 (pathname-name file)))
	      (<> (:text subject)))
	    (<> br)
	    (<> (:text author " on: " date))))))))

;;; ** Web Display / Index
(hunchentoot:define-easy-handler (forum-handler
				  :uri "/") 
    ((location :real-name "location"
	       :init-form 0 
	       :parameter-type 'integer))
  
  (let* ((post-alist (http-parameters-as-alist "post"))
	 (xml (when post-alist (add-post post-alist)))
	 (dir (nreverse (cl-fad:list-directory *storage-pathname*)))
	 (files (subseq dir location (min (+ 10 location)
					  (length dir)))))
    (<> html-sink
      (<> h1 
	(<> "Simple Forum"))
      (display-post-list files)
      (unless (zerop location)
	(let ((prev (princ-to-string (max (- location 10) 0))))
	  (<> (a :href (concatenate 'string "?location=" prev))
	    (<> (:text (concatenate 'string "<< Previous (" prev ")"))))))
      (unless (>= (+ 10 location) (length dir))
	(let ((next (princ-to-string (min (- (length dir) 10) (+ location 10)))))
	  (<> (a :href (concatenate 'string "?location=" next))
	    (<> (:text (concatenate 'string " Next (" next ") >>")))))
	)
      (<> hr)
      (display-post-form))))

;;; ** Web Display / View

(hunchentoot:define-easy-handler (post-handler
				  :uri "/view") 
    ((post :real-name "post"
	   :init-form nil))
  (when post
    (destructuring-bind (time author subject)
	(split-sequence:split-sequence #\| post)
      (let* ((filename (merge-pathnames 
			(make-pathname 
			 :name post)
			*storage-pathname*))
	     (stp-xml (cxml:parse-file 
		       filename 
		       (cxml-stp:make-builder)))
	     (post-element (cxml-stp:first-child stp-xml))
	     (title (format nil "\"~A\" --~A | ~A" subject author "Simple Forum")))
	
	(<> (html-sink :title title)
	  (<> h1 
	    (<> (:text title)))
	  (<> h2  (<>  (:text (local-time:format-timestring 
			       nil (local-time:universal-to-timestamp
				    (parse-integer time))))))
	  (<> big 
	    (<> pre 
	      (<> (:text 				   
		   (cxml-stp:data (cxml-stp:first-child (cxml-stp:find-child-if (cxml-stp:of-name "body" ) post-element)))))))            
	  (<> hr))))))






