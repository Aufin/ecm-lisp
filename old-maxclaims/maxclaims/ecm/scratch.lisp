(defpackage :ecm/scratch
  (:use :cl)
  (:import-from :ecm/ml #:<>))
(in-package :ecm/scratch)


    




   
(hunchentoot:define-easy-handler (find-contracts :uri "/ecm/pongo/find-contracts")
    ()
  (<> '(ecm/ml:page :title "ECM: Find Contracts")
    (<> 'html5:h1 "This is a Test")
    (<> '(html5:div :class "center-block" :width "100%")
      "This is a Test")
    (<report-form>)
    (<> "lorum ipsumlorum ipsumlorum ipsumlorum ipsum  lorum ipsum lorum ipsum lorum ipsumlorum ipsum lorum ipsumlorum ipsumlorum ipsumlorum ipsumlorum ipsum lorum ipsum lorum ipsum lorum ipsumlorum ipsumlorum ipsumlorum ipsum")
    (<> :unescaped '#:|<style>
  .ui-autocomplete-loading {
    background: white url("images/ui-anim_basic_16x16.gif") right center no-repeat;
background-image: url(data:image/gif;base64,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);
  }
  #city { width: 25em; }

* highlight results */
.ui-autocomplete span.hl_results {
    background-color: #ffff66;
}
 

 
/* scroll results */
.ui-autocomplete {
    max-height: 250px;
    overflow-y: auto;
    /* prevent horizontal scrollbar */
    overflow-x: hidden;
    /* add padding for vertical scrollbar */
    padding-right: 5px;
}
 
.ui-autocomplete li {
    font-size: 16px;
}
 
/* IE 6 doesn't support max-height
* we use height instead, but this forces the menu to always be this tall
*/
* html .ui-autocomplete {
    height: 250px;
}


  </style>
  <script>
  var ecm = {
   "person_name" : function (person) {
     name = "";
     start = false;

     if(person.hasOwnProperty('first_name')) {
        name = name.concat(person.first_name);
        start = true;
     }
     
     if(person.hasOwnProperty('last_name')) {
        if (start) { name = name.concat(" ") };
        name = name.concat(person.last_name);
     }
  
    if(person.hasOwnProperty('company_name')) {
      if (start) { name = name.concat(", ") };
      name = name.concat(person.company_name);
    }
    return name;
   }
  }
  $(function() {|
        (js/find-autocomplete (find-contract-object))
 #+(or)
        '|   $( "#city" ).autocomplete({
      source: function( request, response ) {
        $.ajax({
          url: "/ecm/pongo/find",
          dataType: "json",
          data: {
            "find" : JSON.stringify({ 
'_type' : 'contract',
 'where' : {'$or' : { 'contract_number' : request.term ,
                      'contract_number' :  { '$like' : request.term + '%' },
                      'contract_number' : { '$ilike' : '%' + request.term + '%' }

                  }
           },
 'order_by' : { 'contract_number' : { '$desc' : { '$eq' : request.term } },
 'contract_number' : { '$desc' : { '$like' : request.term + '%' } },
 'contract_id' : { '$desc' : true }
 }
                                                       }) 
          },
           
          success: function( data ) {
          alert(typeof data);
            console.log;($.map(data, (function (object) { 
                                      return JSON.stringify(object); })));
            response( $.map(data, (function (object) { 
                                      return '<b>asd</b>' + JSON.stringify(object); }
                      )));
           }


        });
      },
      minLength: 3,
      select: function( event, ui ) {
        log( ui.item ?
          "Selected: " + ui.item.label :
          "Nothing selected, input was " + this.value);
      },
      open: function() {
        $( this ).removeClass( "ui-corner-all" ).addClass( "ui-corner-top" );
      },
      close: function() {
        $( this ).removeClass( "ui-corner-top" ).addClass( "ui-corner-all" );
      }
    }).autocomplete( "instance" )._renderItem = function( ul, item ) {
        return $( "<li></li>" ).data("item.autocomplete", item)
            .append( "<a>" + item.var1 + "<br>" + item.var2 + "</a>")
            .appendTo( ul );
    };| 
        '|
   });
 

  </script>

<div class="ui-widget">
  <label for="city">Contract #: </label>
  <input id="city">
  Powered by <a href="http://geonames.org">geonames.org</a>
</div>
 
<div class="ui-widget" style="margin-top:2em; font-family:Arial">
  Result:
  <div id="log" style="height: 200px; width: 300px; overflow: auto;" class="ui-widget-content"></div>
</div>
 
|)))

(defun call-with-user (functiom)
  (let ((user (hunchentoot:session-value :app-user)))
    (if user
        (maxclaims::call-with-app-user
         user         
         (lambda ()
           (maxclaims::with-udb
             (funcall functiom user))))
        (error "No user available"))))

(hunchentoot:define-easy-handler (pongo-find-handler :uri "/ecm/pongo/find")
    (find)
      (let* ((q (s-sql:sql-compile `(:select (pongo.find ,find))))
             (result (call-with-user (lambda (user) (declare (ignorable user))
                                             (postmodern:query q :single)))))
        (if (eql :null result)
            "[]"
            result)))

(ps:defmacro+ps { (&body plist)
  (let ((end (first (last plist))))
    (assert (eql end '}) nil
            "} is mising. No closing for } ~A ~A" end plist)
  `(ps:create ,@(butlast plist))))

(defun find-contract-object (&optional (term 'request.term))
  `({ 
    :_type "contract"
    :where ({ :contract_number ({ "$ilike" (concatenate 'string "%" ,term "%") })})
    :order_by ({
                "contract_number" ({"$desc" ({"$eq" ,term })})
                "contract_number" ({"$desc" ({"$like" (concatenate 'string ,term "%")})})
                "contract_number" ({"$desc" ({"$ilike" (concatenate 'string ,term "%") })})
                "contract_id" ({"$desc" true })
                })
    
    }))

(defun render-object ()
  '(progn
    (defun render-object (object)
      (let ((span (ps:chain ($ "<div>")
                            (css "display" "inline-block")
                            (css "border-bottom" "1px solid grey")))
            (type object._type))
        (case type
          ("contract"
           (ps:chain (render-contract object)
                     (append-to span)))
          ("person"
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
        (when (ps:!= (typeof person.first_name)
                  "undefined")
          (setf string (string.concat person.first_name)))
        (when (ps:!= (typeof person.last_name)
                  "undefined")
          (when (ps:!= string "")
            (setf string (string.concat "&nbsp;")))
          (setf string (string.concat person.last_name)))
        (when (ps:!= (typeof person.company_name)
                  "undefined")
          (when (ps:!= string "")
            (setf string (string.concat ",&nbsp;")))
          (setf string (string.concat person.company_name)))

        (ps:chain ($"<span>") (append (+ string " ")))))
      
    (defun render-contract (contract)
      (let ((c ($.extend (ps:create) contract)))
        (ps:chain
         ($ "<span>")
         (data "contract-id" c._id)
         (append
          (progn
            (delete c._type)
            (delete c._contract_number)
            (delete c._id)
            (ps:chain
             ($ "<h3>")
             (append (concatenate 'string
                                  c.contract_number " ")))))
         (append (render-object c)))))
                
    ))
 
(defun render-item ()
  '(lambda (ul item)
    (console.log "render")
    (ps:chain
     ($ "<li></li>")
     (append (render-object item))
     (append-to ul))))

(defun js/find-autocomplete
    (find-jso
     &key (selector "#city")
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
          (console.log (+ "selected " (|JSON.stringify| ui)))))
       (render-item (render-item)))
       
  (let ((ajax
         `({  
            :url "/ecm/pongo/find"
            "dataType" "json"
            :data ({ :find (|JSON.stringify| ,find-jso)})
            :success ,success
            })))                    
    (ps:ps*
     '(defvar contracts (ps:array))
     (render-object)
     `(setf (ps:chain
             ($ ,selector)
             (autocomplete 
              ({
                :select ,select
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
                :source (lambda (request response) ($.ajax ,ajax))
          
                }))
             (autocomplete "instance")
             |_renderItem|)
            ,render-item))))

(defun <report-form> ()
  (<> '(form :method "POST" :action "/ecm/create?create[type]=spreadsheet-llyods-bordereau&access[read-only]=false")
    (<> '(input :name "present[CONTRACT]"
          :value "1969"
          :id "contracts"))
    (<> '(input :name "present[START-DATE]"
          :class "datepicker"
          :value "2016-01-01"))
    (<> '(input :name "present[END-DATE]"
          :class "datepicker"
          :value "2016-01-31"))
    (<> '(input :name "present[SPREADSHEET-TYPE]"
          :value "Gnumeric_Excel:xlsx2"))
    (<> '(button :type "submit" :class "btn btn-success")
      (<> "go"))
    (<> 'html5:script
      (ps:ps
        ($ (lambda ()
             (ps:chain ($ ".datepicker")
                       (datepicker))
             (ps:chain ($ ".datepicker")
                       (datepicker
                        "option" "dateFormat" "yy-mm-dd"))))))))
        
    
       
       
        
                   
           
                                       
    
    
    

