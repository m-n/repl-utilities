;;;; utils.lisp

(in-package #:repl-utilities)

;;;; Readtable for this file and repl-utilities.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The resolution of symbols at run time allows repl-utilities
  ;; compiled without quicklisp to call quicklisp at runtime, and a
  ;; fasl compiled with quicklisp to be loaded into an image without
  ;; quicklisp.

  (defvar *repl-utilities-rt* (copy-readtable ())
    "A readtable where [ql quickload] reads as
    `(find-symbol ,(symbol-name 'quickload) ,(symbol-name 'ql))")

  (defun run-time-symbol-reader (stream char)
    (declare (ignore char))
    (destructuring-bind (package name) (read-delimited-list #\] stream)
      `(find-symbol ,(symbol-name name) ,(symbol-name package))))

  (set-macro-character #\[ #'run-time-symbol-reader () *repl-utilities-rt*)
  (set-syntax-from-char #\] #\) *repl-utilities-rt*)
  (setq *readtable* *repl-utilities-rt*))

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

(defun string-sort (list)
  (sort (copy-list list) #'string<))

(defun first-line (string)
  (flet ((min-or-nil (&rest args)
	   (let ((numbers (remove-if-not #'numberp args)))
	     (if numbers (apply 'min numbers) nil))))
    (subseq string 0 (min-or-nil (position #\ string)
				 (position #\Newline string)))))

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
  (unless (find-package "ASDF")
    (return-from load-system-or-print
      (format t "I don't know how to load a system without asdf. ~
                Attempting package manipulation anyway.")))
  (let ((quicklispp (find-package "QUICKLISP")))
    (handler-case
        (if quicklispp
            (funcall [ql quickload]
                     system-designator)
            (funcall [asdf load-system] system-designator))
      (error (c)
        (cond ((and quicklispp
                    (typep c [quicklisp-client system-not-found]))
               (when (string-equal (funcall [ql system-not-found-name] c)
                                   system-designator)
                 (when control-string
                   (apply #'format t control-string format-args))))
              ((typep c `(and ,[asdf missing-component] (not ,[asdf missing-dependency]))) 
               (when control-string
                 (apply #'format t control-string format-args)))
              (t (format t "I'm lost on condition ~A!~&" c)))))))

(defparameter *documentation-types*
  '(function setf type variable compiler-macro ;structure
    #-clisp method-combination)
  "Types that might work with (documentation obj type)")

(defun print-asdf-description (package)
  (let ((description (ignore-errors
                       (funcall [asdf system-description]
                        (funcall [asdf find-system]
                         (string-downcase (package-name
                                           package)))))))
    (when description
      (format t "~&~A > ASDF System~% ~<~A~%~%~>"
              (package-name package) description))))

;;;; Portability

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require 'sb-introspect)
  #+sbcl (require 'sb-sprof))
 
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

