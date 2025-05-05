(defpackage :zamsdeals.nlp
  (:use :cl :str :trivia)
  (:nicknames :nlp)
  (:shadow str:match)
  (:documentation "This package for processing text")
  (:export :remove-punctuation :remove-json-encapsulation :remove-lisp-encapsulation :make-strict-json :tokenize :count-terms :compute-tf :generate-substring-counts))

(defpackage :zamsdeals.fdb
  (:use :cl :fiveam :trivia :local-time :zamsdeals.nlp :foundationdb :cl-hash-util :cl-binary-store :mgl-pax :babel :zsort :split-sequence
	:cl-intbytes)
  (:nicknames :fdb)
  (:local-nicknames (:store :cl-binary-store) (:hash :cl-hash-util))
  (:export :save-item :get-autocomplete-terms :search-items :start-client))

(defpackage :zamsdeals
  (:use :cl :ppcre :com.inuoe.jzon :clog :clog-gui :cl-who :zamsdeals.fdb :cl-hash-util :chronicity)
  (:local-nicknames (:jzon :com.inuoe.jzon) (:ppcre :cl-ppcre))
  (:shadow clog:escape-string clog:hash chronicity:parse chronicity:span))
