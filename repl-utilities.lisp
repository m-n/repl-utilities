;;;; repl-utilities.lisp

(in-package #:repl-utilities)


;; An alternative like (do-external-symbols (sym 'repl-utilities)
;;                       (describe sym))
;; is less nice because it wastes too much vertical space.

(defun readme (&optional (package *package*))
  "Print the documentation on the exported functions of a package."
  (let (undocumented-symbols)
    (do-external-symbols (sym package)
      (unless (some (lambda (doctype) (documentation sym doctype))
		    '(compiler-macro function method-combination
		      setf structure type variable))
	(push sym undocumented-symbols)))
    (when undocumented-symbols
      (format t "~&Undocumented exported symbols:~%~% ~{~A ~}~%~%Documented exported symbols:~%~%"
	      undocumented-symbols)))
  (let ((*print-case* :downcase))
    (do-external-symbols (sym package)
      (dolist (type '(compiler-macro function method-combination  ;structure
		      setf type variable))
	(when (documentation sym type)
	  (if (member type '(compiler-macro function method-combination setf))
	      (format t "~&(~:@(~A~) ~{~A~}) > ~A~% ~<~A~%~%~>"
		       sym (arglist sym) type
		      (documentation sym type))
	      (format t "~&~A > ~A~% ~<~A~%~%~>" sym type (documentation sym type))))))))

(defun exs (&optional (package *package*))
  "Print the external symbols of package."
  (let (ss)
    (do-external-symbols (s package)
      (push s ss))
    (format t "~{~A, ~}" (sort ss #'string<))))

(defun exfns (&optional (package *package*))
  (let (fns)
    (do-external-symbols (fn package)
      (when (fboundp fn)
	(push fn fns)))
    (format t "~{~A, ~}" (sort fns #'string<))))

(defun excs (&optional (package *package*))
  (let (classes)
    (do-external-symbols (class package)
      (when (find-class class nil)
	(push class classes)))
    (format t "~{~A, ~}" (sort classes #'string<))))

#+sbcl
(defun exts (&optional (package *package*))
  (let (types)
    (do-external-symbols (type package)
      (when (sb-ext:valid-type-specifier-p type)
	(push type types)))
    (format t "~{~A, ~}" (sort types #'string<))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro doc (func)
  "Print any documentation for the symbol.
Use variable, function, structure, type, compiler macro, method combinationor, or setf."
  (with-gensyms (arguments)
    (setf arguments (list 'compiler-macro 'method-combination 'variable 'function 'structure 'type 'setf))
    (loop for arg in arguments do
	  (when (documentation func arg)
	    (format t "~a:::  ~s~%~%" arg (documentation func arg))))))

(defun trace-package (&optional (package *package*) (inheritedp nil))
  "Trace all of the symbols in *package*. Don't trace :cl or :cl-user"
  (let ((pac (find-package package)))
    (loop for sym being the symbols in pac when 
	  (if inheritedp
	      t
	      (eq pac (symbol-package sym)))
	  do (ignore-errors (eval `(trace  ,sym))))))

(defun print-hash (hash-table)
  "Print the hash table as: Key, Value~% "
  (loop for k being the hash-keys in hash-table
	do (format t "~A, ~A~%" k (gethash k hash-table))))

(defmacro deflex (var val &optional (doc nil docp))    
  "Define a top level (global) lexical VAR with initial value VAL,
  which is assigned unconditionally as with DEFPARAMETER. If a DOC
  string is provided, it is attached to both the name |VAR| and the
  name *STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of
  kind 'VARIABLE. The new VAR will have lexical scope and thus may be
  shadowed by LET bindings without affecting its dynamic (global) value."
  (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
	 (s1 (symbol-name var))
	 (s2 (symbol-name '#:*))
	 (s3 (symbol-package var))	; BUGFIX [see above]
	 (backing-var (intern (concatenate 'string s0 s1 s2) s3)))
    ;; Note: The DEFINE-SYMBOL-MACRO must be the last thing we do so
    ;; that the value of the form is the symbol VAR.
    (if docp
      `(progn
	 (defparameter ,backing-var ,val ,doc)
	 (setf (documentation ',var 'variable) ,doc)
	 (define-symbol-macro ,var ,backing-var))
      `(progn
	 (defparameter ,backing-var ,val)
	 (define-symbol-macro ,var ,backing-var))))
  ;;; DEFLEX is
  ;;; Copyright (c) 2003-2007, 2011 Rob Warnock <rpw3@rpw3.org>.
  ;;; All Rights Reserved.
  ;;; 
  ;;; Permission to use, copy, modify, and/or distribute this software for any
  ;;; purpose with or without fee is hereby granted, provided that the above
  ;;; copyright notice and this permission notice appear in all copies.
  ;;;
  ;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  ;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  ;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  ;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  ;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
)
(defun de (&rest rest)
  "Shortening of describe. A Rob Warnock function."
  (apply #'describe rest)) 

(defmacro lex (&rest args)
  "Shortening of deflex: define a global lexical variable."
  `(deflex  ,@args))

(defmacro first-form (&rest forms)
  "Return the first form; useful when you want one of multiple possible 
conditionally read forms."
  (first forms))

(defmacro dev (package)
  "Load the package, then swap to it. Import repl-utilities exported symbols that don't conflict.
For use at the repl. Mnemonic for develop."
  `(progn (first-form #+quicklisp (ql:quickload (symbol-name ',package))
		      #+asdf(asdf:load-system ',package))
	  (in-package ,package)
	  (do-external-symbols (sym (find-package 'repl-utilities))
	    (if (find-symbol (symbol-name sym))
		(format t "~&Left behind ~A to avoid conflict.~%" sym)
		(import sym)))))

(defmacro bring (package)
  "Load the package. Import the package's exported symbols that don't conflict.
For use at the repl."
  `(progn (first-form #+quicklisp (ql:quickload (symbol-name ',package))
		      #+asdf(asdf:load-system ',package))
	  (do-external-symbols (sym (find-package (symbol-name ',package)))
	    (if (find-symbol (symbol-name sym))
		(import sym)
		(format t "~&Left behind ~A to avoid conflict.~%" sym)))))

(defvar *advised-functions* (make-hash-table))

(defun advisedp (symbol)
  (nth-value 1 (gethash symbol *advised-functions*)))

(defun add-advice (symbol advice &optional afterp)
  "Add an 'advice' function to be called on the arguments before or after
the symbol-function is called on them."
  (unless (advisedp symbol)
    (setf (gethash symbol  *advised-functions*) (symbol-function symbol)))
  (let ((old-fn (symbol-function symbol)))
    (setf (symbol-function symbol)
       (cond (afterp
	      (lambda (&rest args)
		(prog1 (apply old-fn args)
		  (apply advice args))))
	     (t
	      (lambda (&rest args)
		(apply advice args)
		(apply old-fn args)))))))

(defun remove-advice (&rest functions)
  "Clear the advice from the given functions."
  (loop for fn in functions do
	(progn (setf (symbol-function fn) (gethash fn *advised-functions*))
	       (remhash fn *advised-functions*))))

;; TODO find dependency locations, eg
;; (mapcar (lambda (x) (ql:where-is-system (string-downcase (symbol-name x)))) (cdadr (asdf:component-depends-on 'asdf:load-op (asdf:find-system 'algorithms))))
;; but without ql?
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require 'sb-introspect)
;  #+(or ccl corman) (require 'ccl) ; uncommenting breaks clozure it. 
  #+(or clisp ecl scl) (require 'ext)
  #+abcl (require 'sys)
  #+allegro (require 'excl)
  #+lispworks (require 'lw))
 
(defun arglist (fname)
  "Return the arglist for the given function name.
Implementations taken from slime."
  (or
   #+sbcl (sb-introspect:function-lambda-list fname)
   #+ccl (multiple-value-bind (arglist binding)
	     (let ((*break-on-signals* nil))
	       (ccl:arglist fname))
	   (if binding
	       arglist
	       :not-available))
   #+clisp (block nil
	     (or (ignore-errors
		   (let ((exp (function-lambda-expression fname)))
		     (and exp (return (second exp)))))
		 (ignore-errors
		   (return (ext:arglist fname)))
		 :not-available))
   #+abcl (cond ((symbolp fun)
		 (multiple-value-bind (arglist present) 
		     (sys::arglist fun)
		   (when (and (not present)
			      (fboundp fun)
			      (typep (symbol-function fun) 'standard-generic-function))
		     (setq arglist
			   (mop::generic-function-lambda-list (symbol-function fun))
			   present
			   t))
		   (if present arglist :not-available)))
		(t :not-available))
   #+allegro (handler-case (excl:arglist symbol)
	       (simple-error () :not-available))
   #+cmucl (etypecase fun
	     (function (function-arglist fun))
	     (symbol (function-arglist (or (macro-function fun)
					   (symbol-function fun)))))
   #+corman (handler-case
		(cond ((and (symbolp name)
			    (macro-function name))
		       (ccl::macro-lambda-list (symbol-function name)))
		      (t
		       (when (symbolp name)
			 (setq name (symbol-function name)))
		       (if (eq (class-of name) cl::the-class-standard-gf)
			   (generic-function-lambda-list name)
			   (ccl:function-lambda-list name))))
	      (error () :not-available))
   #+ecl (multiple-value-bind (arglist foundp)
	     (ext:function-lambda-list name)
	   (if foundp arglist :not-available))
   #+lispworks (let ((arglist (lw:function-lambda-list symbol-or-function)))
		 (etypecase arglist
		   ((member :dont-know) 
		    :not-available)
		   (list
		    (replace-strings-with-symbols arglist))))
   #+scl (multiple-value-bind (args winp)
	     (ext:function-arglist fun)
	   (if winp args :not-available))
   :not-available))