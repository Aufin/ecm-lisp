(defpackage :ecm/ui/jquery
  (:use :cl :sexpml)
  (:documentation "Bootstrap Needs < 3.0

 Extensions : https://github.com/xdan/datetimepicker
             
              http://trentrichardson.com/examples/timepicker
              https://cdnjs.com/libraries/jquery-ui-timepicker-addon
              https://cdnjs.com/libraries/chosen
              https://harvesthq.github.io/chosen/
              http://dotdotdot.frebsite.nl/
")
  (:export #:css #:js))
(in-package :ecm/ui/jquery)

(defmethod sexpml:sexpml-form ((name (eql 'css))
                               &key &allow-other-keys)
  `(progn
     (<> :unescaped '#:|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/jqueryui/1.12.1/themes/base/jquery-ui.min.css" integrity="sha256-sEGfrwMkIjbgTBwGLVK38BG/XwIiNC/EAG9Rzsfda6A=" crossorigin="anonymous" />|)
     (<> :unescaped '#:|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/chosen/1.6.2/chosen.min.css" integrity="sha256-QD+eN1fgrT9dm2vaE+NAAznRdtWd1JqM0xP2wkgjTSQ=" crossorigin="anonymous" />|)
     (<> :unescaped '#:|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/jquery-ui-timepicker-addon/1.6.3/jquery-ui-timepicker-addon.min.css" integrity="sha256-AbZqn2w4KXugIvUu6QtV4nK4KlXj4nrIp6x/8S4Xg2U=" crossorigin="anonymous" />|)))

(defmethod sexpml:sexpml-form ((name (eql 'js))
                               &key &allow-other-keys)
  (let ((sexpml:*sexpml-indent* nil))
    `(progn
       (<> :unescaped
         (string '#:|<!-- jQuery library -->
       <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.4.0/jquery.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery-migrate/3.1.0/jquery-migrate.js" integrity="sha256-Yd1j9A67RPnTeoEPIfmCytUkgqVKiX9BcuAKLHQGx+8=" crossorigin="anonymous"></script>
|)
       ,(sexpml:sexpml-form
         'html5:script
         :attributes '(:src "https://code.jquery.com/ui/1.11.4/jquery-ui.min.js"
                       :crossorigin "anonymous"))
       (<> :unescaped
	       '#:|<script src="https://cdnjs.cloudflare.com/ajax/libs/chosen/1.6.2/chosen.jquery.min.js" integrity="sha256-sLYUdmo3eloR4ytzZ+7OJsswEB3fuvUGehbzGBOoy+8=" crossorigin="anonymous"></script>|)
       (<> :unescaped
	       '#:|<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery-ui-timepicker-addon/1.6.3/jquery-ui-timepicker-addon.min.js" integrity="sha256-gQzieXjKD85Ibbpg4l8GduIagpt4oUSQRYaDaLd+8sI=" crossorigin="anonymous"></script>|)))))
