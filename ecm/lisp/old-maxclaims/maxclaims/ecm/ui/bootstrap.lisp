(defpackage :ecm/ui/bootstrap
  (:use :cl :sexpml)
  (:export #:css #:js))
(in-package :ecm/ui/bootstrap)



;; * CSS
(defmethod sexpml:sexpml-form ((name (eql 'css))
                               &key &allow-other-keys)
  "Version 5"
  (sexpml:sexpml-form
   :unescaped
   :contents (list (string '#:|

<!-- Latest compiled and minified CSS -->
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css">|))))
;; * Javascript

(defmethod sexpml:sexpml-form ((name (eql 'js))
                               &key &allow-other-keys)
  "Version 5"
  (sexpml:sexpml-form
   :unescaped
   :contents (list (string '#:|

<!-- Popper JS -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js"></script>
<!-- Latest compiled JavaScript -->
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"></script> |))))

