;;;; package.lisp

(defpackage :repl-utilities
  (:use :cl)
  (:export
   #:dev
   #:bring
   #:print-hash
   #:doc
   #:trace-package
   #:deflex
   #:lex
   #:add-advice
   #:remove-advice
   #:readme
   #-ccl ;; causes a symbol conflict
   #:arglist
   #-ccl ;; ditto
   #:advisedp				; 
   #:de
   #:nic
   #:dependency-locations
   #:*advised-functions*
   #:dbgv
   #:exs
   #:exfns
   #:excs
   #:exts
   ;; coming soon? 
;   #:enable-ppcre-reader
;   #:enable-run-reader
))

