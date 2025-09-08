(defpackage :ecm/ui/report/bordereau-api
  (:use :cl)
  (:import-from :ecm/entity/bordereau
                #:bordereau->json)
  (:import-from :ecm/user #:with-user)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/ui/report
                #:<bordereau-style>
                #:<report-navbar>
                #:list-syndicates)
  (:import-from :ecm/ui/spreadsheet
		#:<spreadsheet-type-select>
		#:<download-spreadsheet>)
  (:import-from :ecm/ui/report/agency-bordereau
                #:list-agencies))
(in-package :ecm/ui/report/bordereau-api)

;; A bordereau has:
;; - a type (lloydsv5, arch, white-oak etc)
;; - a "for" (contract/syndicate/agency)
;; - a date range :: Start Date and End Date
;; - Optionally, a risk type via
;;     (dolist (r (ecm/entity/risk:risk-types)) ...)

(defun bordereau-run-page (num)

  (<> (ecm/ui/page:page :title (format nil "Run BDX ~A" num))
    (<> :text num)
    ))

(defun <bordereau-run-frame> ()
  (<> (div :id "apiRunFrame"
           :style "display:none;text-align:center")
    (<> (form :action "javascript:void(0);")
    (<> '(table :style "margin:auto;" :class "table table-bordered table-sm")
      (<> 'tr (<> 'th) (<> 'td (<> 'h3 "Bordereau Report")))
      (<> '(tr :id "bError" :style "display:none;text-align:center")
        (<> 'th "ERROR:") (<> '(td :id "bErrMsg"
                                :class "alert alert-danger")
                                        "None"))


         (<> 'tr (<> 'th "For:") (<> '(td :id "bFor") ""))
         (<> 'tr (<> 'th "Type:") (<> '(td :id "bType") ""))
         (<> 'tr (<> 'th "Start Date:") (<> (td :id "bStart")
                                          ""))
         (<> 'tr (<> 'th "End Date:") (<> '(td :id "bEnd")
                                        ""))

         (<> 'tr (<> 'th "Started On:") (<> '(td :id "bStartTime")
                                      ""))

      (<> 'tr (<> 'th "Run Time:")
        (<> '(td :id "bRunTimeTD"
              :class "")
          (<> (html5:span :class "toast"
                          :style "position:fixed")
                          "Running...")
            (<> (html5:span :id "bRunTime"))
            ))
         (<> 'tr (<> 'th "Status:") (<> '(td :id "bStatus")
                                      "Unknown"))
      (<> 'tr (<> 'th "Filename:")
        (<> '(td :id "bFilename")
          (<> (input :type "text" :name "bdx-filename"
                     :value "BDX" :class "form-control"
                     :style "text-align:center"))))))

    (<> (div :id "bdxLoading")
      (<> (button :class "btn btn-secondary" "disabled")
        (<> "Loading... ")
        (<> :unescaped '|<div class="spinner-border spinner-border-sm text-light" role="status">
      </div>|)
        )

      (<> (button :class "btn btn-warning":onclick "$('#apiForm').show(); $('#apiRunFrame').hide();")
        (<> "Cancel")))


    (<> (div :id "bdxFinished" :style "display:none")
    (<> (button :class "btn btn-success"
                :onclick "ECM.downloadCurrentBdx()")
      "Download" (<> :unescaped '|<i style="display:none" class="fa fa-check"></i>|))
      (<> (button :class "btn btn-primary"
                  :onclick "ECM.toggleBdx();window.location = '/'")
   (<> "Go to Index >"))
      (<> (button :class "btn btn-info"
                  :onclick "ECM.toggleBdx();$('#apiForm').show(); $('#apiRunFrame').hide();")
   (<> "< Back to Bordereau"))


      )

    )





  (<> 'html5:script
    "globalThis.ECM = typeof ECM == 'undefined' ? {} : ECM;

  ;



$(function () {
ECM.toggleBdx = () => {

$('#bdxFinished button i').hide()
$('#bdxLoading').toggle()
$('#bdxFinished').toggle()

}
ECM.currentBdx = null;
ECM.downloadCurrentBdx = function () {
 this.downloadBdxByName(this.currentBdx.name, $('[name=bdx-filename]').val() + '.csv');
$('#bdxFinished button i').show()
}
function resizeInput() {
    $(this).attr('size', $(this).val().length);
}

$('input[type=\"text\"]')
    // event handler
    .keyup(resizeInput)
    // resize on page load
    .each(resizeInput);
 ECM.refreshBdx = function (bdx) {
   const self = this;
    $.ajax({
        type: \"GET\",
        url: '/ecm/report/api/bordereau/run/'+bdx.name,
        dataType: \"json\",
        success: function(data)
        {
          // alert(JSON.stringify(data));
          if (!!data.error
               || data.status == 'Finished'
               || data.status == \"Error\"
             ) {
            clearInterval(self.runInterval)
           }

          if (data.status === 'Error') {
           data.error = '' + (data.error || '') + data.meta
          }
          self.renderBdx(data)
          self.currentBdx = data;
          if (data.status === 'Finished') {
           ECM.toggleBdx()


          }else {
          $('#bRunTimeTD .toast').toast('show')
          }
        }
    });

 }
 ECM.renderBdx = function (bdx = { status: \"UNKNOWN\" }) {
  const frame = $('#apiRunFrame');
  const form = $('#apiForm');
  const self = this;
  function S(e) { return $(e, frame) };

  function renderTime() {
   if (!bdx.run_start_time) return 0;
   const start = new Date(bdx.run_start_time * 1000),
         end = bdx.run_end_time ? new Date(bdx.run_end_time * 1000) : new Date(),
         diff = end - start,
         rsecs = diff / 1000,
         mins = Math.floor(rsecs / 60),
         msecs = mins * 60,
         secs = rsecs - msecs,
         txt = '' + ((mins > 0) ? mins + ' minute'
                       + (mins > 1 ? 's' : '') +', ' : '') + secs + ' seconds'


   $('#bRunTime').text(txt);


  }

  frame.show(); form.hide()

  if (!!bdx.error) {
   S('#bError').show()
   S('#bErrMsg').text(bdx.error)
 } else {
   S('#bError').hide()
}
  const forName= bdx.for == 'Contract' ? bdx.json.contract_number : bdx.json.short_name;
  S('#bFor').text(bdx.for + \": \" + forName)
  S('#bType').text(bdx.type)
  S('#bStatus').text(bdx.status)
  S('#bStart').text(bdx.start_date)
  S('#bEnd').text(bdx.end_date)

  if (!!bdx.run_start_time) {
    S('#bStartTime').text(new Date(bdx.run_start_time * 1000))
     renderTime()
  }

  $('#bFilename input').val((
   bdx.type + ' for ' + bdx.for + ' ' + forName
   + ' from ' + bdx.start_date + ' to ' + bdx.end_date).replace('.',''))

}

})"))

(defun <select-bordereau-type> (&key (active 'lloyds-v5)
                                  (name "bordereau-type")
                                  (id "bdxType")
                                  (class "form-control text-center"))
  (<> `(select :name ,name
               :id ,id
               :class ,class
		           :style "display:inline-block")

    (dolist (s (ecm/entity/bordereau:list-bordereau-types))
      (declare (type symbol s active))
	    (<> `(option :value ,s
			             ,@(when (string= active s)
				               (list :selected "selected")))
		    (<> :text (format nil "~:(~a~)" (substitute #\Space #\-
                                                    (symbol-name s))))))))
(defun <select-bordereau-for> (&key (active 'syndicate)
                                  (name "bordereau-for")
                                 (id "bdxFor")
                                 (class "form-control text-center"))
  (<> `(select :name ,name
               :id ,id
               :class ,class
		           :style "display:inline-block")

    (dolist (s (ecm/entity/bordereau:list-bordereau-fors))
      (declare (type symbol s active))
	    (<> `(option :value ,s
			             ,@(when (string= active s)
				               (list :selected "selected")))
		    (<> :text (format nil "~:(~a~)" (substitute #\Space #\-
                                                    (symbol-name s))))))))

(defun bordereau-api-page
    (&key
			 (error nil)
       (type nil)
			 (page-title "Bordereau API")
			 (navbar-title page-title)
			 (spreadsheet-type "Gnumeric_Excel:xlsx2")
			 (contract-id nil contract-id-provided?)
			 (agency-id nil agency-id-provided?)
       (syndicate-id nil syndicate-id-provided?))
  (<> (ecm/ui/page:page :title page-title)
    (<bordereau-style>)
    (<bordereau-run-frame>)
    (let ((contract (when contract-id
		                  (ecm/entity/contract:find-contract contract-id))))
      (<report-navbar> navbar-title)
      (when error
        (<> (div :class "alert alert-danger"
	               :style "max-height: 5em; overflow: auto;")
	        (<> (strong) "ERROR:")
	        (<> :text (format nil "~A" error))))
      (<> (br))

	    ;; (<> (strong) "Herenow")
      (<> (form :method "POST"
	              :target "_blank"
                :id "apiForm"
	              :action (concatenate 'string "/ecm/report/api/bordereau/" type))

        ;; (<> :text (ecm/hunchentoot:post-parameters*))
        (<> (input :type "hidden" :name "contract-id" :id "contractID"
		               :value contract-id))
        (<> '(div :class "row"
              :style "width:100%;text-align:center;")

          (<> '(div :class "col-md")
            (<> 'h3 "Bordereau:")
            (<select-bordereau-type>)
            (<> '(div)
              (<> 'h5 (<> "Start Date"))
              (<> '(input :name "start-date"
		                :style "text-align:center;"
                    :class "datepicker form-control"
                    :id "startDate")))
            (<> '(div)
              (<> 'h5 (<> "End Date (excluding)"))
              (<> '(input :name "end-date"
		                :style "text-align:center;"
                    :class "datepicker form-control"
                    :id "endDate")))
            (<> '(div )
            (<> 'h5 (<> "Risk Type"))
            (ecm/ui/risk:<select-risk-type>
	           nil :style "text-align:center"
	           :all-risks t))
            )
          (<> '(div :class "col-md")
            (<> (div :style "margin-bottom:0.5em")
              (<> 'h3 "For:")
              (<select-bordereau-for>))
            (<> '(div :id "contractShow")
              (ecm/ui/contract:<contract-autocomplete>
               :display-style "" :width "100%")
	            (<> (span :id "contractHide")
	              (<> '(input :id "contract" :class "form-control"
		                  :style "display:inline-block"
                      :placeholder "Enter Contract #"))
	              (<> '(div :id "contractAutoAppend"
		                  :onload "this.width=$(\".container\").width();")))
	            (when contract
	              (ecm/ui/contract:<contract-display> contract))
	            (when (and (not contract)
		                     contract-id-provided?)
	              (<> (div :class "alert alert-danger"
		                     :id "contractInvalid")
	                (<> (strong) "Invalid Contract!")
	                (<> " A contract must be provided to continue"))))
            (when t
              (<> '(div :id "syndicateShow")
	              (ecm/user:with-user ()
	                (<> '(select :name "syndicate-id"
                        :class "form-control text-center"
		                    :style "display:inline-block")
                    (<> (option :value "") "")
	                  (dolist (s (list-syndicates))
		                  (<> `(option :value ,(second s)
			                             ,@(when (equalp (princ-to-string (second s))
					                                         syndicate-id)
				                               (list :selected "selected")))
		                    (<> :text (first s))))))))
            (<> '(div :id "agencyShow")
	            (<> '(select :name "agency-id"
                    :class "text-center form-control")
	              (dolist (s (list-agencies))
		              (<> `(option :value ,(second s)

			                         ,@(when (equalp (princ-to-string (second s))
					                                     agency-id)
				                           (list :selected "selected")))
		                (<> :text (first s))))))
            (<> 'hr)
            (<> '(button :type "submit"
	                  :class "btn btn-success"
	                  :name "new-window"
	                  :value "true")
	              (<> "Open Report in New Window"))
            )
          )
        (<> '(div :class "row" :style "width:100%;text-align:center;")

         
         )

      (<> 'hr)
        (<> 'html5:script
          " globalThis.ECM = typeof ECM == 'undefined' ? {} : ECM;

 ECM.universalTime = function (secs) {
    var t = new Date('1900-01-01T00:00:00Z'); // Epoch
    t.setSeconds(secs);
    return t;
 } ;
 ECM.displayAPI = function (){
    const fors = [
      \"contractShow\", \"syndicateShow\" , \"agencyShow\"];
        fors.map(fr => $($(\"#\" + fr)[0]).hide())
     let bFor = $('#bdxFor')[0].value.toLowerCase()
      $('#' + bFor + 'Show').show();
 }

 ECM.downloadBdxByName = function (name, fn = 'bdx.csv') {
    const url = '/ecm/report/api/bordereau/download/' + name +
      '?bdx-filename='+ fn;

    window.location = url
 }

$(function () {
  ECM.displayAPI();
  $('#bdxFor')[0].onchange = (event) => {
    ECM.displayAPI();
  };

  // this is the id of the form
  $(\"#apiForm\").submit(function(e) {

    e.preventDefault(); // avoid to execute the actual submit of the form.

    var form = $(this);
    var actionUrl = form.attr('action');

    $.ajax({
        type: \"POST\",
        url: window.location.href,
        dataType: \"json\",
        data: form.serialize(), // serializes the form's elements.
        success: function(data)
        {
          // alert(JSON.stringify(data));
          ECM.renderBdx(data)
          ECM.runInterval = setInterval(function () { ECM.refreshBdx(data) }, 1000)
        }
    });

});
  });

")
        (<> 'html5:script
          (ps:ps
            ($ (lambda ()
	               (ps:chain
                  ($".ecm-select")
                  (selectmenu
				           ({} "width" "100%")))
                 (ps:chain ($ ".datepicker")
                           (datepicker))
                 (ps:chain ($ ".datepicker")
                           (datepicker
                            "option" "dateFormat" "yy-mm-dd"))
                 (let* ((date (ps:new (|Date|)))
                        (month (ps:chain date (get-month)))
                        (year (ps:chain date (get-full-year)))
                        )
		               (|.| date (set-date 1))
		               (when (= "" (|.| ($ "#endDate") (val)))
		                 (ps:chain ($ "#endDate")
			                         (datepicker "setDate" date)))
                   (|.| date  (set-month
                               (if (= month 0)
                                   11
                                   (ps:decf month))))
                   (|.| date (set-year
                              (if (= month 0)
                                  (decf year)
                                  year)))
                   (when (= "" (|.| ($ "#startDate") (val)))
                     (ps:chain ($ "#startDate")
                               (datepicker "setDate" date))))))))

        ))))
