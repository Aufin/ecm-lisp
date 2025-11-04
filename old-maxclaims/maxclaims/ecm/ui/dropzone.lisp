(defpackage :ecm/ui/dropzone
  (:use :cl :sexpml)
  (:export #:js #:css))
(in-package :ecm/ui/dropzone)

(defmethod sexpml:sexpml-form ((name (eql 'js))
                               &key &allow-other-keys)
  `(<> :unescaped
     ,(string '|<script src="https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.3.0/dropzone.js" integrity="sha256-vnXjg9TpLhXuqU0OcVO7x+DpR/H1pCeVLLSeQ/I/SUs=" crossorigin="anonymous"></script>|)))

(defmethod sexpml:sexpml-form ((name (eql 'css))
                               &key &allow-other-keys)
  `(<> :unescaped
     ,(string '|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.3.0/dropzone.css" />|)))
	      

