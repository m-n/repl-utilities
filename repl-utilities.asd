;;;; repl-utilities.asd

(asdf:defsystem #:repl-utilities
  :serial t
  :description "Ease common tasks at the REPL."
  :license "BSD 2-clause"
  :author "Matt Niemeir"
  :components ((:file "package")
	       (:file "utils")
               (:file "repl-utilities")))
