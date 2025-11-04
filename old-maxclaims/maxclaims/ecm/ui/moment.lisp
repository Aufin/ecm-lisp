(defpackage :ecm/ui/moment
  (:use :cl :sexpml)
  (:documentation "Moment.js 
Parse, validate, manipulate, and display dates in JavaScript.
http://momentjs.com/
")
  (:export #:js))
(in-package :ecm/ui/moment)

(defmethod sexpml:sexpml-form ((name (eql 'js))
                               &key &allow-other-keys)
  (let ((sexpml:*sexpml-indent* nil))
    `(progn
       ,(sexpml:sexpml-form
         'html5:script
         :attributes '(:src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.14.1/moment-with-locales.min.js"
                       :crossorigin "anonymous"))
              (<> :unescaped
	 '#:|<script src="https://cdnjs.cloudflare.com/ajax/libs/moment-timezone/0.5.9/moment-timezone-with-data.min.js" integrity="sha256-fzcxI4qEYFEu1vQISpQNdTa81uG0VB0dMkz4Q1v36ZU=" crossorigin="anonymous"></script>|))))


                      

