(defpackage :ecm/ui/pdfjs
  (:use :cl :sexpml)
  (:documentation "
https://mozilla.github.io/pdf.js/

A general-purpose, web standards-based platform for parsing and rendering PDFs.

https://cdnjs.com/libraries/pdf.js


")
  (:export #:js))
(in-package :ecm/ui/pdfjs)

(defmethod sexpml:sexpml-form ((name (eql 'js))
                               &key &allow-other-keys)
  '(<> :unescaped '#:|<script src="https://cdnjs.cloudflare.com/ajax/libs/pdfobject/2.0.201604172/pdfobject.min.js" integrity="sha256-dUe7c2jPkbycNGu4OSy/3FbLKU5FK1aM62Vc5Ni7sgs=" crossorigin="anonymous"></script>|))


                      

