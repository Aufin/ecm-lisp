(defpackage :ecm/ui/autocomplete
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{})
  (:export #:<autocomplete-style>
           #:js/find-autocomplete
	   #:render-object))
(in-package :ecm/ui/autocomplete)

(defun render-object ()
  '(progn
    (defun render-object (object)
      (let ((span (ps:chain ($ "<div>")
                            (css "display" "inline-block")
                            ))
            (type object._type))
        (case type
          ("contract"
           (ps:chain (render-contract object)
                     (append-to span)))
          ("person"
           (ps:chain (render-person object)
                     (append-to span)))
	  ("corpus_summary"
           (ps:chain (render-person object)
                     (append-to span)))
          (otherwise
           ($.map object
                  (lambda (v k)
                    (ps:chain
                     ($ "<span>")
                     (css "display" "inline-block")
                     (css "margin-right" "1em")
                     (append
                      (ps:chain ($ "<small class=\"text-muted\">")

                                (append (+ k " "))))
                     (append (if (equal (ps:typeof v) "object")
                                 (render-object v)
                                 (+ v " ")))
                     (append-to span))))))
        span))

    (defun render-person (person)
      (let ((string ""))
        (when (ps:!== (typeof person.first_name)
                  "undefined")
          (setf string (string.concat person.first_name)))
        (when (ps:!== (typeof person.last_name)
                  "undefined")
          (when (ps:!== string "")
            (setf string (string.concat "&nbsp;")))
          (setf string (string.concat person.last_name)))
        (when (ps:!== (typeof person.company_name)
                  "undefined")
          (when (ps:!== string "")
            (setf string (string.concat ",&nbsp;")))
          (setf string (string.concat person.company_name)))
	(when (ps:!== (typeof person.province)
                  "undefined")
          (when (ps:!== string "")
            (setf string (string.concat ",&nbsp;")))
          (setf string (string.concat person.province.short_name)))

        (ps:chain ($"<span>") (append (+ string " ")))))
      
    (defun render-contract (contract)
      (let ((c ($.extend (ps:create) contract)))
        (ps:chain
         ($ "<span>")
         (data "contract-id" c._id)
         (append
          (let ((cn (concatenate 'string
                                  c.contract_number " ")))
            (delete c._type)
            (delete c.contract_number)
            (delete c._id)
            (ps:chain
             ($ "<h3>")
             (append cn))))
         (append (render-object c)))))))
 
(defun render-item ()
  '(lambda (ul item)
    (ps:chain
     ($ "<li></li>")
     (append (render-object item))
     (append-to ul))))

(defun js/find-autocomplete
    (find-jso
     &key (selector "#city")
       (append-to nil)
       (position nil)
       (min-length 3)
       (success
        '(lambda (data)
          ;; (alert (typeof data))
          (console.log (+ "found " data))
          (response data)))
       (select
        '(lambda (event ui)
          (contracts.push ui.item._id)
          (ps:chain ($ "#contracts") (val contracts))
          (ps:chain ($ ".report") (show))
          (ps:chain ($ "#listed-contracts")
           (append 
            (ps:chain ($"<div>") (append (render-object ui.item)))))))
       (render-item (render-item)))
  (let ((ajax
         `({} 
            :url "/ecm/api/find"
            "dataType" "json"
            :data ({} :find (|JSON.stringify| ,find-jso))
            :success ,success)))
    (ps:ps*
     '(defvar contracts (ps:array))
     (render-object)
     `(setf (ps:chain
             ($ ,selector)
             (autocomplete
              ({}
                :select ,select
                ,@(when position (list :position position))
                ,@(when append-to (list "appendTo" append-to))
                "minLength" ,min-length
                :open (lambda ()
                        (ps:chain
                         ($ this)
                         (remove-class "ui-corner-all")
                         (add-class "ui-corner-top")))
                :close (lambda ()
                         (ps:chain
                          ($ this)
                          (remove-class "ui-corner-top")
                          (add-class "ui-corner-all")))
                :source (lambda (request response) ($.ajax ,ajax))))
             (autocomplete "instance")
             |_renderItem|)
            ,render-item))))

(defun <autocomplete-style> ()
    (<> :unescaped '#:|<style>
  .ui-autocomplete-loading {
    background: white url("images/ui-anim_basic_16x16.gif") right center no-repeat;
background-image: url(data:image/gif;base64,R0lGODlhEAAQAMQAAP///+7u7t3d3bu7u6qqqpmZmYiIiHd3d2ZmZlVVVURERDMzMyIiIhEREQARAAAAAP///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh+QQFBwAQACwAAAAAEAAQAAAFdyAkQgGJJOWoQgIjBM8jkKsoPEzgyMGsCjPDw7ADpkQBxRDmSCRetpRA6Rj4kFBkgLC4IlUGhbNQIwXOYYWCXDufzYPDMaoKGBoKb886OjAKdgZAAgQkfCwzAgsDBAUCgl8jAQkHEAVkAoA1AgczlyIDczUDA2UhACH5BAUHABAALAAAAAAPABAAAAVjICSO0IGIATkqIiMKDaGKC8Q49jPMYsE0hQdrlABCGgvT45FKiRKQhWA0mPKGPAgBcTjsspBCAoH4gl+FmXNEUEBVAYHToJAVZK/XWoQQDAgBZioHaX8igigFKYYQVlkCjiMhACH5BAUHABAALAAAAAAQAA8AAAVgICSOUGGQqIiIChMESyo6CdQGdRqUENESI8FAdFgAFwqDISYwPB4CVSMnEhSej+FogNhtHyfRQFmIol5owmEta/fcKITB6y4choMBmk7yGgSAEAJ8JAVDgQFmKUCCZnwhACH5BAUHABAALAAAAAAQABAAAAViICSOYkGe4hFAiSImAwotB+si6Co2QxvjAYHIgBAqDoWCK2Bq6A40iA4yYMggNZKwGFgVCAQZotFwwJIF4QnxaC9IsZNgLtAJDKbraJCGzPVSIgEDXVNXA0JdgH6ChoCKKCEAIfkEBQcAEAAsAAAAABAADgAABUkgJI7QcZComIjPw6bs2kINLB5uW9Bo0gyQx8LkKgVHiccKVdyRlqjFSAApOKOtR810StVeU9RAmLqOxi0qRG3LptikAVQEh4UAACH5BAUHABAALAAAAAAQABAAAAVxICSO0DCQKBQQonGIh5AGB2sYkMHIqYAIN0EDRxoQZIaC6bAoMRSiwMAwCIwCggRkwRMJWKSAomBVCc5lUiGRUBjO6FSBwWggwijBooDCdiFfIlBRAlYBZQ0PWRANaSkED1oQYHgjDA8nM3kPfCmejiEAIfkEBQcAEAAsAAAAABAAEAAABWAgJI6QIJCoOIhFwabsSbiFAotGMEMKgZoB3cBUQIgURpFgmEI0EqjACYXwiYJBGAGBgGIDWsVicbiNEgSsGbKCIMCwA4IBCRgXt8bDACkvYQF6U1OADg8mDlaACQtwJCEAIfkEBQcAEAAsAAABABAADwAABV4gJEKCOAwiMa4Q2qIDwq4wiriBmItCCREHUsIwCgh2q8MiyEKODK7ZbHCoqqSjWGKI1d2kRp+RAWGyHg+DQUEmKliGx4HBKECIMwG61AgssAQPKA19EAxRKz4QCVIhACH5BAUHABAALAAAAAAQABAAAAVjICSOUBCQqHhCgiAOKyqcLVvEZOC2geGiK5NpQBAZCilgAYFMogo/J0lgqEpHgoO2+GIMUL6p4vFojhQNg8rxWLgYBQJCASkwEKLC17hYFJtRIwwBfRAJDk4ObwsidEkrWkkhACH5BAUHABAALAAAAQAQAA8AAAVcICSOUGAGAqmKpjis6vmuqSrUxQyPhDEEtpUOgmgYETCCcrB4OBWwQsGHEhQatVFhB/mNAojFVsQgBhgKpSHRTRxEhGwhoRg0CCXYAkKHHPZCZRAKUERZMAYGMCEAIfkEBQcAEAAsAAABABAADwAABV0gJI4kFJToGAilwKLCST6PUcrB8A70844CXenwILRkIoYyBRk4BQlHo3FIOQmvAEGBMpYSop/IgPBCFpCqIuEsIESHgkgoJxwQAjSzwb1DClwwgQhgAVVMIgVyKCEAIfkECQcAEAAsAAAAABAAEAAABWQgJI5kSQ6NYK7Dw6xr8hCw+ELC85hCIAq3Am0U6JUKjkHJNzIsFAqDqShQHRhY6bKqgvgGCZOSFDhAUiWCYQwJSxGHKqGAE/5EqIHBjOgyRQELCBB7EAQHfySDhGYQdDWGQyUhADs=);
  }

* highlight results */
.ui-autocomplete span.hl_results {
    background-color: #ffff66;
}
 

 
/* scroll results */
.ui-autocomplete {
    max-height: 75vh;
    overflow-y: auto;
    /* prevent horizontal scrollbar */
    overflow-x: hidden;
    /* add padding for vertical scrollbar */
    padding-right: 5px;
}
 
.ui-autocomplete li {
    font-size: 16px;
}
 
}


  </style>|))



   



        
    
       
       
        
                   
           
                                       
    
    
    

