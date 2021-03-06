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
   #:package-apropos
   #-(or clisp ccl) ;; causes a symbol conflict
   #:arglist
   #:de
   #:nic
   #:dependency-locations
   #:dbgv
   #:repeatably
   #:rig
   #:exs
   #:exfns
   #:exvs
   #:excs
   #:exts
   #:mac
   #:shadowed-import))
