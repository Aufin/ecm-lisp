(uiop:define-package :ecm/ui/html
    (:use :cl :sexpml/html5)
  (:import-from :sexpml/html5)
  (:reexport :sexpml/html5)
  (:export #:page))
(in-package :ecm/ml)

(defmethod sexpml:sexpml-form ((name (eql 'page))
                               &key attributes contents)
  ;(decf sexpml::*sexpml-indent-level* (min sexpml::*sexpml-indent-level* 2))
  (sexpml-attributes-bind ((title nil) (div-class "container")) attributes
    `(with-output-to-string (*sexpml-output*)
       ,(sexpml:sexpml-form
         'html5:html
         :attributes '(:doctype nil)
         :contents
         `((<> 'html5:head
             , (when title
                 `(<> 'html5:title ,title))
               (<> 'ecm/bootstrap:css)
               (<> 'ecm/jquery:css)
               (<> 'ecm/jquery:js)
               (<> 'ecm/bootstrap:js))
           (<> 'html5:body
             ,(sexpml:sexpml-form
               'html5:div
               :attributes '(:class "container")
               :contents contents)))))))

