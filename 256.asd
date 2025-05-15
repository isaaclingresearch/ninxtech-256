(defsystem "256"
  :author "Ninx Technology Limited <info@ninx.xyz>"
  :description "Advertise deals, find deals in a specified georange."
  :version "0.0.2"
  :depends-on (:str :trivia :cl-ppcre
		    ;; sort
		    :zsort
		    ;; tests
		    :fiveam
		    ;; encode decode utils
		    :flexi-streams :cl-hash-util :com.inuoe.jzon :babel :split-sequence :trivia
		    ;; time utils
		    :local-time :chronicity
		    ;; web dev utils
		    :cl-who :parenscript :hunchentoot :easy-routes
		    ;; storage packages
		    :postmodern
		    ;; documentation packages
		    :mgl-pax :3bmd :colorize
		    )
  :components ((:module "src"
		:components ((:file "package")
			     (:file "doc")
			     (:file "db")
			     (:file "server")
			     (:file "app")
			     )))
  :build-operation "program-op" ;; leave as is
  :build-pathname "256"
  :entry-point "256:start-server")
