(defpackage :ecm/ui/datatables
  (:use :cl :sexpml)
  (:export #:css #:js))
(in-package :ecm/ui/datatables)

(defmethod sexpml:sexpml-form ((name (eql 'css))
                               &key &allow-other-keys)
  (sexpml:sexpml-form
   :unescaped
   :contents (list (string '|

<link rel="stylesheet" href="//cdn.datatables.net/1.10.19/css/jquery.dataTables.min.css" />
<link rel="stylesheet" href="//cdn.datatables.net/responsive/2.2.2/css/responsive.bootstrap4.min.css" />
|))))

(defmethod sexpml:sexpml-form ((name (eql 'js))
                               &key &allow-other-keys)
  (sexpml:sexpml-form
   :unescaped
   :contents (list (string '|

<script src="//cdn.datatables.net/1.10.19/js/jquery.dataTables.min.js"></script>

<script src="//cdn.datatables.net/1.10.18/js/dataTables.bootstrap4.min.js"></script>

<script src="//cdn.datatables.net/responsive/2.2.2/js/dataTables.responsive.min.js"></script>
<script src="//cdn.datatables.net/responsive/2.2.2/js/responsive.bootstrap4.min.js"></script>


</script>

|))))

