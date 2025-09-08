(defpackage :ecm/ui/tether
  (:use :cl :sexpml)
  (:documentation "https://cdnjs.com/libraries/tether")
  (:export #:js))
(in-package :ecm/ui/tether)


(defmethod sexpml:sexpml-form ((name (eql 'js))
                               &key &allow-other-keys)
  (sexpml:sexpml-form
   :unescaped
   :contents (list (string '#:|<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/tether/1.3.2/js/tether.min.js"></script>|))))
