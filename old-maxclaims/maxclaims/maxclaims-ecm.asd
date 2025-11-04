;;; -*- lisp -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :maxclaims/asdf-system)
    (defpackage :maxclaims/asdf-system
      (:documentation "ASDF Systems for \"Maxclaims\"")
      (:use :common-lisp :asdf)
      (:export #:project-relative-pathname))))

(in-package :maxclaims/asdf-system) 

(defun project-relative-pathname (path)
  (merge-pathnames path (component-pathname (find-system :maxclaims))))



(defsystem :maxclaims-ecm
  :version "4.0.0"
  :components 
  (;; This should not be in the maxclaims.asd at all but /is/ so
   ;; cannot be here.
   #+(or)(:module "ucw"
		  :components
		  ((:file "application")))
   (:module :src
	    :components
	    ((:module :yaclml 
		      :components
		      ((:file "tags")))
	     (:file "hunchentoot")
	     (:module :lib
		      :components
		      ((:file "db-utils")))
	     (:file "database")
	     (:module :log
		      :components 
		      ((:file "user-log")
		       (:file "user-history")
		       (:file "object-history")))
	     (:module :data-entities 
		      :pathname "data-entity"
		      :components
		      ((:file "app-user")
		       (:file "app-user-message")
		       (:file "ibc-code"))
		      :depends-on ("database"))
	     (:module :all-reports
		      :pathname "report"
		      :components ((:file "cheque-register")
				   (:file "claim-authority")
				   (:file "bordereau")
				   (:file "timecard")
				   (:file "time-recorded")
				   (:file "open-claim")
				   (:file "interim"))
		      :depends-on (:data-entities))
	     (:file "ecm-description" :depends-on (:yaclml
						   :data-entities))

	     (:module :ecm-descriptions 
		      :pathname "ecm-description"
		      :components
		      ((:file "claim" :depends-on ("timecard"))
		       (:module :claims
				:pathname "claim"
				:components
				((:file "detail")
				 (:file "cause")
				 (:file "update")
				 (:file "attachment")
				 (:file "authority"))
				:depends-on ("claim"))
		       (:module :contracts
				:pathname "contract"
				:components
				((:file "authority"))
				:depends-on ("contract"))
		       (:module :reports
				:pathname "report"
				:components
				((:file "open-claim")
				 (:file "interim")
				 (:file "time-recorded")
				 (:file "claim-authority")
				 (:file "timecard")
				 (:file "spreadsheet")
				 (:file "spreadsheet/white-oak-bordereau")
                                 (:file "spreadsheet/llyods-bordereau")
				 (:file "spreadsheet/arch-bordereau"))
				:depends-on ("claim"))
		       (:file "contract")
		       (:file "policy")
		       (:file "risk")
		       (:file "person")
		       (:file "app-user")
		       (:file "app-user-message")
		       (:file "timecard")
		       (:file "diary")
		       (:file "report")
		       (:file "ibc-code")
		       (:file "report-bordereau")
		       (:file "manage"))
		      :depends-on ("ecm-description" 
				   :log  "database" :all-reports))
	     (:file "text-display"
		    :depends-on (:ecm-descriptions))
	     (:module 
	      :web-display
	      :components ((:file "display")
			   (:file "as-table" :depends-on ("display"))
			   (:file "attachment" :depends-on ("display"))
			   (:file "diary" :depends-on ("display"))
			   (:file "html-page" :depends-on ("display" "inline-edit"))
	
			   (:file "navbar" :depends-on ("html-page"))
			   (:file "inline-edit" :depends-on ("display"))
			   (:file "search" :depends-on ("html-page"
							"navbar"))
			   (:file "history" :depends-on ("navbar"))
			   (:file "view" :depends-on ("navbar"
						      "view-tabs"))
			   (:file "view-tabs" :depends-on ("navbar"))
			   (:file "index" :depends-on ("navbar" "view"))

			   (:file "edit" :depends-on ("navbar" "html-page"))
			   (:file "timecard" :depends-on ("navbar" "view"))
			   (:file "report" :depends-on ("view"))
			   )
	      
	      :depends-on (:yaclml "ecm-description" 
				   "text-display" 
				   :log :lib
				   "hunchentoot"))

		      
	     (:module :entry-point
		      :components
		      ((:file "toplevel")
		       (:file "view" :depends-on ("toplevel"))
		       (:file "view-tabs" :depends-on ("toplevel"))
		       (:file "history" :depends-on ("toplevel"))
		       (:file "index" :depends-on ("view"))
		       (:file "delete" :depends-on ("toplevel"))
		       (:file "create" :depends-on ("toplevel"))
		       (:file "edit" :depends-on ("create"))
		       (:file "download" :depends-on ("toplevel"))
		       (:file "spreadsheet" :depends-on ("toplevel"))
		       (:file "reports" :depends-on ("spreadsheet"))
		       (:file "message" :depends-on ("create"))
		       (:file "login" :depends-on ("toplevel"))
		       (:file "forgot-password" :depends-on ("toplevel"))
		       (:file "diary" :depends-on ("toplevel"))
		       (:file "todo")
		       (:file "inline-edit" :depends-on ("edit"))
		       (:file "timecard" :depends-on ("toplevel"))
		       (:file "manage" :depends-on ("toplevel"))

		       (:file "search"  :depends-on ("toplevel")))
		      :depends-on (:yaclml :web-display :ecm-descriptions
					   :all-reports)))))
	    :serial t
	     
	    :depends-on (:ecm/print
                         :maxclaims
			             :hunchentoot :split-sequence :max-ecm :max-ecm/gnumeric/spreadsheet :max-ecm/gnumeric/ssconvert :max-ecm/json
			             :max-ecm/client :max-ecm/request-context :ecm/request-user
			 :max-ecm/display
			 :max-ecm/output-formatting
			 :max-ecm :max-ecm/html/page
			 :max-ecm/html/login
			 :max-ecm/html/forgot-password
			 :max-ecm/mail

			 :ecm
			 :ecm/hunchentoot
			 :ecm/entity/diary
			 :ecm/endpoint/redirect
                         :ecm/ui/report/transaction-bordereau
                         :ecm/ui/report/temple-transaction-bordereau
			 :ecm/ui/report/mi
                                        ;:ecm/llyods-report 
                                        ;:max-ecm/entity/diary-entry
                                        ;:max-ecm/html/breadcrumbs
                                        ;:sexpml
			 :yasexml)
	    :perform
	    (load-op
	     :after (o c)
	     (asdf:load-system :ecm/ui/report/agency-bordereau)
	     (asdf:load-system :ecm/ui/report/time-recorded)
	     (asdf:load-system :ecm/ui/claim)
	     (asdf:load-system :ecm/endpoint/claim)
	     (asdf:load-system :ecm/endpoint/timecard)
	     (asdf:load-system :ecm/endpoint/thread)
	     (asdf:load-system :ecm/endpoint/ui)
	     (asdf:load-system :ecm/endpoint/timezone)
	     (asdf:load-system :ecm/endpoint/transaction)
	     (asdf:load-system :ecm/endpoint/cheque-register)
	     (asdf:load-system :ecm/endpoint/risk)
	     (asdf:load-system :ecm/endpoint/diary)
	     (asdf:load-system :ecm/endpoint/pillar-3)
	     (asdf:load-system :ecm/ui/timecard)
	     (asdf:load-system :ecm/hack/login)
	     (asdf:load-system :ecm/endpoint/timecard))
		     
			      
			      
			      
	    :version "1.0")

(asdf:register-system-packages
 :maxclaims-ecm '(:maxclaims-ecm :maxclaims/hunchentoot))
