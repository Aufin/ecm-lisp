(defpackage :ecm/ui/report/static-claims
  (:use :cl)
  (:import-from :ecm/entity/api/static)
  (:import-from :ecm/user #:with-user)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ui/report
                #:<report-navbar>)
		
                
  (:import-from :ecm/ps #:{} #:|.|))

(in-package :ecm/ui/report/static-claims)

(defun static-claims-page ()
  (<> (ecm/ui/page:page :title "Static Claims")
    (<report-navbar> "Static Claims")
    (<> (div :style "height: 90vh; margin:auto; text-align:center;")
      (<> (div :class "my-auto")
      (<> (form :method "POST")
      (<> "Show me claims that have not had any movememt for ")
	(<> (input :type "text" :name "interval":value "3 months"))
	(<> 'br)
	(<> (input :type "submit" :value "Download"))

    )))))
  
