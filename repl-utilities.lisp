;;;; repl-utilities.lisp

(in-package #:repl-utilities)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; these may be called in macro definitions
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
  ) ; end eval-when

;;;; Package Utilities

(defmacro dev (package)
  "Load the package, then swap to it. Import repl-utilities exported symbols that don't conflict.
For use at the repl. Mnemonic for develop."
  `(progn (first-form #+quicklisp (ql:quickload (symbol-name ',(ensure-unquoted package)))
		      #+asdf(asdf:load-system ',(ensure-unquoted package))
		      (error 'package-error "~&DEV requires either quicklisp or ~
                                             asdf be present."))
	  (in-package ,(ensure-unquoted package))
	  (do-external-symbols (sym (find-package 'repl-utilities))
	    (if (find-symbol (symbol-name sym))
		(format t "~&Left behind ~A to avoid conflict.~%" sym)
		(import sym)))))

(defmacro bring (package &optional (shadowing-import nil))
  "Load the package. Import the package's exported symbols that don't conflict.
For use at the repl."
  (with-gensyms (gpackage)
    `(let ((,gpackage ',(ensure-unquoted package)))
       (first-form #+quicklisp (ql:quickload (symbol-name ,gpackage))
		   #+asdf (asdf:load-system ,gpackage))
       (let ((,gpackage (find-package ,gpackage)))
	 (do-external-symbols (sym ,gpackage)
	   (if (not ,shadowing-import)
	       (if (find-symbol (symbol-name sym))
		   (unless (eq  (symbol-package
				 (find-symbol (symbol-name sym)))
				,gpackage)
		     (format t "~&Left behind ~A to avoid conflict.~%" sym))
		   (import sym))
	       (shadowing-import sym)))))))

(defmacro readme (&optional (package *package*))
  ;; TODO: optional ansi coloring, sort the symbols in some sensical way, paging?
  "Print the documentation on the exported functions of a package."
  (with-gensyms (undocumented-symbols sym type)
    `(let (,undocumented-symbols)
       (do-external-symbols (,sym ',(ensure-unquoted package))
	 (unless (some (lambda (doctype) (documentation ,sym doctype))
		       '(compiler-macro function method-combination
			 setf structure type variable))
	   (push ,sym ,undocumented-symbols)))
       (when ,undocumented-symbols
	 (format t "~&Undocumented exported symbols:~%~% ~{~A ~}~%~%~
                   Documented exported symbols:~%~%"
		 ,undocumented-symbols))
       (let ((*print-case* :downcase))
	 (do-external-symbols (,sym ',(ensure-unquoted package))
	   (dolist (,type '(compiler-macro function method-combination ;structure
			   setf type variable))
	     (when (documentation ,sym ,type)
	       (if (member ,type '(compiler-macro function method-combination setf))
		   (format t "~&(~:@(~A~)~@[~{ ~A~}~]) > ~A~% ~<~A~%~%~>"
			   ,sym
			   (when #1=(arglist ,sym) (if (consp #1#) #1# (list #1#)))
			   ,type
			   (documentation ,sym ,type))
		   (format t "~&~A > ~A~% ~<~A~%~%~>" ,sym ,type (documentation ,sym ,type))))))))))

(defmacro exs (&optional (package *package*))
  "Print the external symbols of package."
  (with-gensyms (ss s)
    `(let (,ss)
       (do-external-symbols (,s ',(ensure-unquoted package))
	 (push ,s ,ss))
       (format t "~{~A, ~}" (sort ,ss #'string<)))))

(defmacro exfns (&optional (package *package*))
  "Print the external fboundp symbols of a package."
  (with-gensyms (fns fn)
    `(let (,fns)
       (do-external-symbols (,fn ',(ensure-unquoted package))
	 (when (fboundp ,fn)
	   (push ,fn ,fns)))
       (format t "~{~A, ~}" (sort ,fns #'string<)))))

(defmacro excs (&optional (package *package*))
  "Print the external symbols for which find-class is truthy."
  (with-gensyms (classes class)
    `(let (,classes)
      (do-external-symbols (,class ',(ensure-unquoted package))
	(when (find-class ,class nil)
	  (push ,class ,classes)))
      (format t "~{~A, ~}" (sort ,classes #'string<)))))

#+(or sbcl ccl)
(defmacro exts (&optional (package *package*))
  "Print the external symbols which are type specifiers."
  (with-gensyms (types type)
    `(let (,types)
       (do-external-symbols (,type ',(ensure-unquoted package))
	(when (first-form #+sbcl(sb-ext:valid-type-specifier-p ,type)
			  #+ccl(ccl:type-specifier-p ,type))
	  (push ,type ,types)))
      (format t "~{~A, ~}" (sort ,types #'string<)))))

(defmacro trace-package (&optional (package *package*) (inheritedp nil))
  "Trace all of the symbols in *package*. 

 This won't attempt to trace any symbols in :cl"
  (with-gensyms (pac sym cl)
    `(let ((,pac (find-package ',(ensure-unquoted package)))
	   (,cl (find-package :cl)))
      (loop for ,sym being the symbols in ,pac when
	    (unless (eq ,cl (symbol-package ,sym))
	      (if ,inheritedp
		t
		(eq ,pac (symbol-package ,sym))))	    
	    do (ignore-errors (eval `(trace  ,,sym)))))))

(defmacro nic (package-name nick-symbol)
  "Add an additional nickname to package."
  (with-gensyms (old-nicknames)
    `(let ((,old-nicknames (package-nicknames ',(ensure-unquoted package-name))))
       (if (and (find-package ',(ensure-unquoted nick-symbol))
		(not (eq (find-package ',(ensure-unquoted nick-symbol))
			 (find-package ',(ensure-unquoted package-name)))))
	   (format t "Not adding that nick because it belongs to ~A~%"
		   (find-package ',(ensure-unquoted nick-symbol)))
	   (rename-package ',(ensure-unquoted package-name)
			   ',(ensure-unquoted package-name)
			   (cons ',(ensure-unquoted nick-symbol)
				 ,old-nicknames))))))

;;;; Symbol Utilities

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

(defmacro lex (&rest args)
  "Shortening of deflex: define a global lexical variable."
  `(deflex  ,@args))

(defun de (&rest rest)
  "Shortening of describe. A Rob Warnock function."
  (apply #'describe rest))

(defmacro doc (func)
  "Print any documentation for the symbol.
Includes variable, function, structure, type, compiler macro, method
 combination, and setf documentation."
  `(loop for arg in '(compiler-macro method-combination variable
		      function structure type setf)
	 when (documentation ',(ensure-unquoted func) arg) do
	 (format t "~a: ~s~%~%"
		 arg
		 (documentation ',(ensure-unquoted func) arg))))

;;;; Advice
;;; TODO: these advice functions are all alpha quality, and I need to review
;;; the history of functionality typically given with advice functions.

(defvar *advised-functions* (make-hash-table :test #'eq)
  "Hash table of [Key <> Value] of 
[symbol-of-advised-function <> symbol-function-before-advice]")

(defun advisedp (symbol)
  "Returns true if symbol has advice attached."
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
  "Clear all advice from given functions, or from all functions if none named."
  (if functions
      (loop for fn in functions do
	    (progn (setf (symbol-function fn) (gethash fn *advised-functions*))
		   (remhash fn *advised-functions*)))
      (progn (maphash (lambda (fn-symbol unadvised-fn)
			(setf (symbol-function fn-symbol) unadvised-fn))
		      *advised-functions*)
	     (clrhash *advised-functions*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require 'sb-introspect)
;  #+(or ccl corman) (require 'ccl) ; uncommenting breaks clozure.
  #+(or clisp ecl scl) (require 'ext)
  #+abcl (require 'sys)
  #+allegro (require 'excl)
  #+lispworks (require 'lw))
 
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

;;;; Miscellaneous                                                                

(defmacro dbgv ((&optional (where "DEBUG") 
                           (stream '*standard-output*)) 
                &body forms) 
  "Print WHERE, execute FORMS, and print each form and its result to the STREAM."
  ;; Alteration of Maciej Katafiasz's alteration of a Rob Warnock utility
  ;; See http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/df43ce7017c3f101/fda9d18d8196c41b
  (with-gensyms (result) 
    `(let (,result) 
       (progn 
         (format ,stream "~&DBGV at: ~a:~%" ,where) 
         ,@(loop for form in forms 
              collect `(progn 
                         (setf ,result (multiple-value-list ,form)) 
                         (format ,stream "~&~s = ~{~s~^, ~}~%" ',form ,result))) 
         (values-list ,result)))))

(defun print-hash (hash-table)
  "Print the hash table as: Key, Value~% "
  (loop for k being the hash-keys in hash-table
	do (format t "~A, ~A~%" k (gethash k hash-table))))

;; TODO find dependency locations, eg
;; (mapcar (lambda (x) (ql:where-is-system (string-downcase (symbol-name x)))) (cdadr (asdf:component-depends-on 'asdf:load-op (asdf:find-system 'algorithms))))
;; but without ql?

;#+quicklisp

;; (defun dependency-locations (system-name)
;;   "Takes a string as system-name. Only \"works\" and only on QL systmes."
;;   (labels ((rec (deplist)
;; 	     (if (consp deplist)
;; 		 (progn (rec (car deplist)) (rec (cdr deplist)))
;; 	         (when deplist (print (ql:where-is-system
;; 				       (ql::system-file-name deplist)))))))
;;     (rec (ql::dependency-tree system-name))))


(defmacro mac (expr)
  "Bind *gensym-counter* to 0, Macroexpand-1 the form, pprint result.

  If expression starts with a quotation, unquotes it first."
  ;; From On Lisp, modified to bind *gensym-counter* and use ensure-unquoted
  `(let ((*gensym-counter* 0)) ; would setq be preferable?
    (pprint (macroexpand-1 ',(ensure-unquoted expr)))))