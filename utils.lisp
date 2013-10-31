;;;; utils.lisp

(in-package #:repl-utilities)

;;;; General Utilities

(defun ensure-unquoted (form) 
  "If form is quoted, remove one level of quoting. Otherwise return form.
This is a useful for defining convenience for macros which may be passed a
quoted or unquoted symbol."
  (if (and (listp form) (eq (car form) 'cl:quote))
      (second form)
      form))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect
	       ;; (SYMBOL-NAME #:SYMBOL-NAME-2983)
	       `(,n (gensym ,(concatenate 'string (symbol-name n) "-"))))
     ,@body))

(defmacro first-form (&rest forms)
  "Return the first form; useful when you want one of multiple possible 
conditionally read forms."
  (first forms))

(defun shadowed-import (symbols
			&optional (package *package*) (print-when-shadowed-p t))
  "Import each symbol into PACKAGE, unless a symbol of the same name is present.
  If print-when-shadowed-p is true, print a message for each not-imported 
  symbol indicating that it was not imported."
  (dolist (sym (if (consp symbols) symbols (list symbols)) t)
    (let ((found (find-symbol (symbol-name sym) package)))
      (if (not found)
	  (import sym package)
	  (when (and print-when-shadowed-p
		     (not (eq found sym)))
	    (format t "~&Left behind ~A to avoid symbol conflict.~%" sym))))))

(defun load-system-or-print (system-designator &optional control-string
			  &rest format-args)
  (handler-case 	      
      (first-form #+quicklisp (ql:quickload
			       system-designator)
		  #+asdf (asdf:load-system system-designator))
    ((or #+asdf asdf:missing-component
	 #+quicklisp quicklisp-client::system-not-found )
      (c)
      (declare (ignorable c))
      (when control-string
	(apply #'format t control-string format-args)))))

;;;; Portability

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require 'sb-introspect)
  #+sbcl (require  'sb-sprof))
 
(defun arglist (fname)
  "Return the arglist for the given function name.
Implementations taken from slime."
  (first-form
   #+sbcl (sb-introspect:function-lambda-list fname)
   #+ccl (multiple-value-bind (arglist binding)
	     (let ((*break-on-signals* nil))
	       (ccl:arglist fname))
	   (declare (ignore binding))
	   arglist)
   #+clisp (block nil
	     (or (ignore-errors
		  (let ((exp (function-lambda-expression fname)))
		    (and exp (return (second exp)))))
		 (ignore-errors
		  (return (ext:arglist fname)))
		 :not-available))
   :arglist-nonportable-patches-welcome))

