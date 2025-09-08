(defpackage :ecm/ui/pdfobject
  (:use :cl :sexpml)
  (:documentation "

https://pdfobject.com/
https://cdnjs.com/libraries/pdfobject

An open-source standards-friendly JavaScript utility for embedding PDF files into HTML documents.

")
  (:export #:js))
(in-package :ecm/ui/pdfobject)

(defmethod sexpml:sexpml-form ((name (eql 'js))
                               &key &allow-other-keys)
  '(<> :unescaped '#:|<script src="https://cdnjs.cloudflare.com/ajax/libs/pdfobject/2.0.201604172/pdfobject.min.js" integrity="sha256-dUe7c2jPkbycNGu4OSy/3FbLKU5FK1aM62Vc5Ni7sgs=" crossorigin="anonymous"></script>|))


                      

