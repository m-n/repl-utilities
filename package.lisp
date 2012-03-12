;;;; package.lisp

(defpackage :repl-utilities
  (:use :cl)
  (:export
   :dev
   :bring
   :print-hash
   :doc
   :trace-package
;   :initiate-critic ;; where did this come from?
   :deflex
   :lex
   :add-advice
   :remove-advice
   :readme
   #-ccl ;; causes a symbol conflict
   :arglist
   :de))

