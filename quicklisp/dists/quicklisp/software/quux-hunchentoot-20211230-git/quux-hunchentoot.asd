(defsystem "quux-hunchentoot"
  :version "1.0.2"
  :description "Thread pooling for hunchentoot"
  :author "Francois-Rene Rideau"
  :license "MIT"
  :depends-on ((:version :hunchentoot "1.2.17")
               "alexandria" ;; for various utilities
               "bordeaux-threads" ;; for threads
               "lil" ;; for FIFO queues
               "lparallel" ;; for communication channels
               "trivia") ;; for parsing messages sent over channels
  :components
  ((:file "pkgdcl")
   (:file "thread-pooling" :depends-on ("pkgdcl"))))
