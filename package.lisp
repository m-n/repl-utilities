;;;; package.lisp

(defpackage :repl-utilities
  (:use :cl)
  (:export
   #:dev
   #:*dev-hooks*
   #:bring
   #:*bring-hooks*
   #:print-hash
   #:doc
   #:trace-package
   #:deflex
   #:lex
   #:add-advice
   #:remove-advice
   #:readme
   #-(or clisp ccl) ;; causes a symbol conflict
   #:arglist
   #-ccl ;; ditto
   #:advisedp				; 
   #:de
   #:nic
   #+asdf
   #:dependency-locations
   #:*advised-functions*
   #:dbgv
   #:exs
   #:exfns
   #:excs
   #+(or sbcl ccl) ;; needs implementation specific TYPE-SPECIFIER-P or similar
   #:exts
   #:mac))

