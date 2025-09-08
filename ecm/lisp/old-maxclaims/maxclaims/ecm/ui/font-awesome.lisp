(defpackage :ecm/ui/font-awesome
  (:use :cl :sexpml)
  (:documentation "http://fontawesome.io/icons/
")
  (:export #:css))
(in-package :ecm/ui/font-awesome)

(defmethod sexpml:sexpml-form ((name (eql 'css))
                               &key &allow-other-keys)
  (let ((sexpml:*sexpml-indent* nil))
    (sexpml:sexpml-form
       'html5:link
       :attributes '(:href "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
		     :rel "stylesheet"
		     :crossorigin "anonymous"))))


                      

