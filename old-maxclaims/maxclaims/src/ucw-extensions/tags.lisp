(in-package :ucw-standard)

(deftag-macro <ucw::javascript-popup (&attribute action action* function (name "diary")
				 &allow-other-attributes others
				 &body body)
  "A Simple <:A which does not require javascript."
  (%with-action-unique-names 
   `(<:script 
     :type "text/javascript"
     (<:as-is "window.open('"
	      (print-uri-to-string ,url)
	      "', '"
	      ,name "', 'height=600, width=800, scrollbars=1');")
	 ,@others
	 ,@body)))

