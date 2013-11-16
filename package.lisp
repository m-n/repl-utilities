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
   #:readme
   #:summary
   #-(or clisp ccl) ;; causes a symbol conflict
   #:arglist
   #:de
   #:nic
   #+asdf
   #:dependency-locations
   #:dbgv
   #:repeatably
   #+sbcl
   #:rig
   #:exs
   #:exfns
   #:exvs
   #:excs
   #+(or sbcl ccl) ;; needs implementation specific TYPE-SPECIFIER-P or similar
   #:exts
   #:mac
   #:shadowed-import))

