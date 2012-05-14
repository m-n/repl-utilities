;;;; repl-utilities.asd

(asdf:defsystem #:repl-utilities
  :serial t
  :description "Ease common tasks at the REPL."
;  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "repl-utilities")))

