(uiop:define-package :ecm/ui/claim/clone
    (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$ #:$. )
  (:import-from :ecm/ui/utility #:cat)
  (:import-from :ecm/user
		            #:user #:user-id)
  (:import-from :ecm/ui/contract
                #:<contract>
		            #:<select-contracts>)
  (:import-from :ecm/json
		#:getjso)
  (:export #:<clone-claim-modal> #:clone-claim-page))

(in-package :ecm/ui/claim/clone)

(defun clone-claim-page (claim-id)
  (<> (ecm/ui/page:page :title "clone claim")
    (let* ((claim-id (ignore-errors (parse-integer claim-id)))
	         (claim-crux-json
	           (postmodern:query
	            (:select (:jsi.claim-crux claim-id))
	            :single))
	         (claim
	           (progn (unless claim-crux-json
		                  (error "No CLaim Crux for ~A" claim-id))
		                (ecm/json:read-json-from-string
		                 claim-crux-json)))
           (risk (getjso "risk" claim))
           (contract (getjso "contract" risk)))
      (<> (style) "#ecmBody { height: 100% };")

		  ;;(<select-contracts> :selected contract)
      (<> (input :type "hidden" :name "contract-id" :id "contractID"
		             :value (getjso "_id" contract)))
      (<> (div :class "row align-items-center")
        (<> (div :class "col")
          (<> 'h5 (<> "Change Contract")))
        (<> '(div :class "col-8" :id "contractShow")
	        (<> (span :id "contractHide" :style "margin-left:10px")
	          (<> '(input :id "contract" :class "form-control"
		              :style "display:inline-block"))
	          (<> '(div :id "contractAutoAppend"
		              :onload "this.width=$(\".container\").width();")))
	        (when contract
	          (ecm/ui/contract:<contract-display> contract))
          (ecm/ui/contract:<contract-autocomplete>)
          ))
      (<> (div :class "row align-items-center " )
        (<> (div :class "col")
          (<> 'h5 (<> "Subscription %")))
        (<> '(div :class "col-8")
          (<> (input :type "text" :class "form-control"
                     :name "sub-percent" :id "SubPer"
		             :value (getjso "subscription_percent" claim)))
	        ))
      (<> (hr))
      (<> (div :class "row align-items-center justify-content-between"
               :id "cloneSubmitDiv")
        (<> :unescaped '|<div class="alert alert-danger"
                              id="cloneError" role="alert"
                              style="display:none">
  This is a danger alert—check it out!
</div>|)
        (<> (div :class "col-4"
                 :id "cloneBack"
                 :style "display:none")
          (<> (button :class "btn btn-outline-primary float-right"
                        :data-dismiss "modal"
                      :onclick "window.parent.$('#ecmModal').modal('hide'); window.parent.location.reload()"
                      )
            (<> :text "Back to #" claim-id))
       )
        (<> (div :class "col-4")
          (<> (button :class "btn btn-outline-success"
                      :onclick "submitCloneClaim()")
            "Submit and Clone"))
        (<> (div :class "col-4")
          (<> (button :class "btn btn-outline-danger float-right"
                      :data-dismiss "modal"
                      :id "cloneCancel"
                      :onclick "window.parent.$('#ecmModal').modal('hide')"
                      )
            "Cancel")
            )
        )

      (<> (script)
        "window.submitCloneClaim = () => {
   const vals = [\"SubPer\", \"contractID\"]
     .map(s => document.getElementById(s).value),
         args = { subscription: vals[0],
                  \"contract-id\": vals[1]
                }

        $('#cloneError').hide();
        $.post(window.location, args,
           function (data, txtStatus, xhr) {

         console.log(\"Got\", xhr.status, data)

          const obj = data[0] === '{'
                ? JSON.parse(data)
                : {},
               { claim_id } = obj;

        $('#cloneCancel').hide();
        $('#cloneBack').show();
        window.parent.$('#ecmModal').on('hide.bs.modal', function (e) {
         window.parent.location.reload()
        })
        $('#cloneSubmitDiv').prepend(
          '<div> Cloned to #' + claim_id +'</div>')

        

      }).fail(function({ responseText, status }, textStatus, errorThrown)
    {
       const obj = responseText[0] === '{'
                ? JSON.parse(responseText)
                : responseText,
             txt = typeof obj === 'string' ? obj : obj.error;

        console.error(\"Error posting Clone: \", obj);


        $('#cloneError').text(txt).show();
    });
}"

        ))))


(defun claim-iframe-src (claim-id)
  (format nil "<iframe src=\"/ecm/claim/~A/clone\"\
  style=\"border:none;width:100%;height:100%\"
    onload=\"window.parent.$('#cloneIFrameLoading').hide()\"></iframe>"
          claim-id)
  )
(defun <clone-claim-modal> (claim-id)
  (<> (script)
    (ps:ps
      (defun redir-clone ()
	      (alert "refresh!"))
      (defvar clone-claim ({})))
    (ps:ps*
     `($ (lambda ()
	         (let* ((modal ($ "#ecmModal"))
		              (title ($. modal (find ".modal-title")))
		              (body ($. modal (find ".modal-body")))
		              (div ($ "#claimRisk")))

	           (defun show-clone-claim ()
	             (let ((iframe ,(claim-iframe-src claim-id)))
		             ($. body (empty)  (append "<div id=\"cloneIFrameLoading\" style=\"font-size:200%;width:100%\" class=\"text-xs-center\"><i  class=\"fa fa-spinner fa-spin spin-normal\"></i></div>")
                     (append iframe))
		             ($. title (empty) (append (+ "Cloning " ,claim-id)))
		             ($. modal (modal "show"))))

             (setf clone-claim
                   ({} "show" show-clone-claim))
	           ))))))
