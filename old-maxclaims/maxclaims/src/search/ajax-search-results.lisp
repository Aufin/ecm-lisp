(in-package #:maxclaims)

;;; Initial purely ajax search results
;;; Will need generalizing, but it's a start

;;; actions for ajax....
;;; - generate json of results

(defun xhr-search-results-source (self)
  (make-action
   (lambda ()
     (setf (get-header *response* "Content-Type") "application/json")
     (json:encode-json-plist
      (list :results (map 'list
			  (lambda (res)
			    (list (cons "type" (class-name (class-of res)))
				  (cons "result"
					(with-inactive-descriptions (html-description
								     editable)
					  (with-active-descriptions (inline)
					    (display nil res))))
				  (cons "viewAction"
					(action-href
					 ;; export ucw::make-action-body?
					 (make-action (lambda ()
							(arnesi:with-call/cc
							  (search-link-action self res))))))))
			  (paged-set.data self)))
      (html-stream *response*)))
   :call-render nil
   :make-new-frame nil))

(define-display
  :in-description ajax-description
  ((desc search-results) (self search-results) displayed-object)
  (declare (ignorable displayed-object))
  (unless (paged-set.emptyp self)
    (with-unique-dom-ids (container-dom-id)
      (let ((callback-url (action-href (xhr-search-results-source self)))) 
	(<:div :id container-dom-id :class "yui-skin-sam"
	       (with-inactive-descriptions (ajax-description)
		 (call-next-layered-method)))
	(<js:script
	 `(*maxclaims-yui-loader*.really-insert
	   (array "connection" "paginator" "datatable")
	   (lambda ()
	     (defvar results-data-source (new (*YAHOO*.util.*x-h-r-data-source
					       (+ window.location.protocol
						  "://"
						  window.location.hostname
						  ,callback-url))))
	     (setf results-data-source.response-schema
		   (create :results-list "results"
			   :fields (array "type" "result" "viewAction")))
	     (defvar results-table
	       (new (*YAHOO*.widget.*data-table
		     ,container-dom-id
		     (array (create :key "type")
			    (create :key "result"
				    :formatter (lambda (el rec col data)
						 (let ((link (document.create-element "a")))
						   (link.set-attribute "href"
								       (rec.get-data "viewAction"))
						   (link.append-child (document.create-text-node
								       data))
						   (el.append-child link)))))
		     results-data-source
		     (create :paginator (new (*yahoo*.widget.*paginator
					      (create :rows-per-page 25))))))))))))))

