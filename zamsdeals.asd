(defsystem "zamsdeals"
  :author "Ninx Technology Limited <info@ninx.xyz>"
  :description "Advertise deals, find deals in a specified georange."
  :version "0.1.0"
  :depends-on (:str :trivia :cl-ppcre
		    ;; sort
		    :zsort
		    ;; tests
		    :fiveam
		    ;; encode decode utils
		    :flexi-streams :cl-hash-util :com.inuoe.jzon :babel :split-sequence
		    ;; time utils
		    :local-time :chronicity
		    ;; web dev utils
		    :cl-who :clog :drakma :cl-html-parse
		    ;; storage packages
		    :postmodern
		    ;; documentation packages
		    :mgl-pax :3bmd :colorize
		    )
  :components ((:module "src"
		:components ((:file "package")
			     )))
  :build-operation "program-op" ;; leave as is
  :build-pathname "zamsdeals"
  :entry-point "zamsdeals:start-server")
