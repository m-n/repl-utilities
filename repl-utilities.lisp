;;;; repl-utilities.lisp

(in-package #:repl-utilities)

;;;; Package Utilities

(defvar *dev-hooks* ()
  "List of functions to be funcalled after a package is loaded with DEV.

 During execution of these functions *PACKAGE* is already set to the package
 being loaded, and the repl-utilities symbols which will be imported already
 are. The functions are called with no arguments.")

(defmacro dev (package)
  "Attempt to ql:quickload or asfd:load-system a system with the same name as
   package, then swap to the package regardless of whether the load was
   successful. Import repl-utilities exported symbols that don't cause symbol
   conflicts into the newly swapped to pacage.
Mnemonic for develop.

  After swapping to the package map funcall over *dev-hooks*."
  (with-gensyms (gpackage start)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (prog ((,gpackage ',(ensure-unquoted package)))
	  ,start
	  (load-system-or-print ,gpackage
				"~&Could not find system, ~
                              attempting to ~
                              in-package anyway.~%")
	  (restart-case (setq *package* (or (find-package ,gpackage)
					    (find-package (string-upcase (string ,gpackage)))
					    (error "No package named ~A found."
						   ,gpackage)))
	    (specify-other-package ()
	      :report "Specify an alternate package name: "
	      (setq ,gpackage
		    (ensure-unquoted (read)))
	      (go ,start)))
	  (do-external-symbols (sym (find-package 'repl-utilities))
	    (shadowed-import sym *package* t))
	  (map nil #'funcall *dev-hooks*)))))

(defvar *bring-hooks* ()
  "List of functions to be funcalled after a package is loaded with BRING.

 The functions are called with the package imported by bring as their only 
 argument.")

(defmacro bring (package &optional (shadowing-import nil))
  "Attempt to ql:quickload or asdf:load-system a system with the same name as 
  package. Regardless of whether the load was successful import the package's
  exported symbols into the current package. If shadowing-import is nil, only
  the symbols which won't cause a symbol conflict are imported.

  After importing the package funcall each element of *bring-hooks* with the
  package as its argument.

  Expands to an EVAL-WHEN :compile-toplevel :load-toplevel :execute"
  (with-gensyms (gpackage start)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (prog ((,gpackage ',(ensure-unquoted package)))
	  ,start
	  (load-system-or-print ,gpackage "~&System not found, attempting ~
                                       to import symbols from package ~
                                       ~A if it exists.~%" ,gpackage)
	  (restart-case
	      (or (find-package ,gpackage)
		  (find-package (string-upcase (string ,gpackage)))
		  (error "No package named ~A found."
			 ,gpackage))
	    (specify-other-package ()
	      :report
	      "Specify an alternate package name: "
	      (setq ,gpackage
		    (ensure-unquoted (read)))
	      (go ,start)))
	  (do-external-symbols (sym ,gpackage)
	    (if (not ,shadowing-import)
		(shadowed-import sym *package* t)
		(shadowing-import sym)))
	  (map nil (lambda (fn) (funcall fn ,gpackage)) *bring-hooks*)))))

(defmacro readme (&optional (package *package*))
  ;; TODO: optional ansi coloring, sort the symbols in some sensical way, paging?
  "Print the documentation on the exported symbols of a package.

  Expands to an EVAL-WHEN :compile-toplevel :load-toplevel :execute"
  (with-gensyms (undocumented-symbols sym)
    `(let (,undocumented-symbols)
       (terpri)
       (when (documentation (find-package ',(ensure-unquoted package)) t)
	 (format t "~&~A > Package~% ~<~A~%~%~>"
		 ',(ensure-unquoted package)
		 (documentation (find-package ',(ensure-unquoted package)) t)))
       #+asdf
       (when (and (ignore-errors (asdf:find-system
				  (string-downcase (package-name
						    ',(ensure-unquoted package)))))
		  (ignore-errors
		   (asdf:system-description
		    (asdf:find-system
		     (string-downcase (package-name
				       ',(ensure-unquoted package)))))))
	 (format t "~&~A > ASDF System~% ~<~A~%~%~>"
		 (package-name ',(ensure-unquoted package))
		 (asdf:system-description
		    (asdf:find-system
		     (string-downcase (package-name
				       ',(ensure-unquoted package)))))))
       (do-external-symbols (,sym ',(ensure-unquoted package))
	 (unless (some (lambda (doctype) (documentation ,sym doctype))
		       '(compiler-macro function #-clisp method-combination
			 setf structure type variable))
	   (push ,sym ,undocumented-symbols)))
       (when ,undocumented-symbols
	 (format t "~&Undocumented exported symbols:~%~% ~{~A ~}~%~%~
                   Documented exported symbols:~%~%"
		 ,undocumented-symbols))
       (do-external-symbols (,sym ',(ensure-unquoted package))
	 (doc% ,sym)))))

(defmacro summary (&optional (package *package*))
  "Print the exported symbols along with the first line of their docstrings."
  `(summary% ',(ensure-unquoted package)))

(defun summary% (&optional (package-designator *package*))
  (let ((types (list 
		(list 'function) (list 'setf) (list 'type) (list 'variable)
		(list 'compiler-macro) #-clisp (list 'method-combination)))
	(undocumented-symbols ()))
    (do-external-symbols (symbol package-designator)
      (push symbol undocumented-symbols)
      (dolist (type (mapcar #'car types))
	(when (documentation symbol type)
	  (push symbol (cdr (assoc type types)))
	  (setq undocumented-symbols (remove symbol undocumented-symbols)))))
    (when undocumented-symbols
      (format t "~&Undocumented symbols: ~{~A~^, ~}" undocumented-symbols))
    (map nil (lambda (field)
	       (destructuring-bind (type . symbols) field
		 (format t "~&")
		 (when symbols
		   (format t "~%~:(~A~)s" type)
		   (mapc (lambda (symbol)
			   (format t "~&~A:~20,5t~a~%"
				   symbol (first-line
					   (documentation symbol type))))
			 symbols))))
	 types)))

(defun first-line (string)
  (flet ((min-or-nil (&rest args)
	   (let ((numbers (remove-if-not #'numberp args)))
	     (if numbers (apply 'min numbers) nil))))
    (subseq string 0 (min-or-nil (position #\ string)
				 (position #\Newline string)))))

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
  "Add an additional nickname to package.
  Expands to an EVAL-WHEN :compile-toplevel :load-toplevel :execute"
  (with-gensyms (old-nicknames)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,old-nicknames (package-nicknames
			      ',(ensure-unquoted package-name))))
	 (if (and (find-package ',(ensure-unquoted nick-symbol))
		  (not (eq (find-package ',(ensure-unquoted nick-symbol))
			   (find-package ',(ensure-unquoted package-name)))))
	     (format t "Not adding that nick because it belongs to ~A~%"
		     (find-package ',(ensure-unquoted nick-symbol)))
	     (rename-package ',(ensure-unquoted package-name)
			     ',(ensure-unquoted package-name)
			     (cons ',(ensure-unquoted nick-symbol)
				   ,old-nicknames)))))))

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

(defmacro doc (symbol &rest ignored-arguments)
  "Print any documentation for the symbol.
Includes variable, function, type, compiler macro, method
 combination, and setf documentation."
  (declare (ignore ignored-arguments))
  `(doc% ',(ensure-unquoted symbol)))

(defun doc% (symbol)
  (do ()
      ((not (consp symbol)))
    (setq symbol (car symbol)))
  (let ((*print-case* :downcase))
    (dolist (type '(compiler-macro function setf type variable ;structure
		    #-clisp method-combination))
      (when (documentation symbol type)
	(if (member type '(compiler-macro function setf
			   #-clisp method-combination))
	    (format t "~&(~:@(~A~)~@[~{ ~A~}~]) > ~A~% ~<~A~%~%~>"
		    symbol
		    (when #1=(arglist symbol)
			  (if (consp #1#) #1# (list #1#)))
		    (if (macro-function symbol)
			'macro
			type)
		    (documentation symbol type))
	    (format t "~&~A > ~A~% ~<~A~%~%~>"
		    symbol
		    type
		    (documentation symbol type)))))))

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

#+asdf
(defun dependency-locations (system-name &optional
					   print-system-names-p
					   (operation 'asdf:load-op))
  "Print the pathname of the system and of the systems needed to operation it.

  Operation should be a symbol naming an operation recognized by
  asfd:component-depends-on, e.g. 'asdf:load-op or 'asfd:test-op."
  (let (printed-systems)
    (labels ((rec (sys)
	       (setq sys (asdf:find-system sys))
	       (unless (member sys printed-systems)
		 (push sys printed-systems)
		 (format t "~&~S" (asdf:component-pathname sys))
		 (when print-system-names-p
		   (format t ", ~A~&"  (asdf:component-name sys)))
		 (map nil
		      #'rec
		      (cdr (find operation
				 (asdf:component-depends-on operation sys)
				 :key #'car))))))
      (rec system-name))
    ;(nreverse printed-systems)
    ))

(defmacro mac (expr)
  "Bind *gensym-counter* to 0, Macroexpand-1 the form, pprint result.

  If expression starts with a quotation, unquotes it first."
  ;; From On Lisp, modified to bind *gensym-counter* and use ensure-unquoted
  `(let ((*gensym-counter* 0) ; would setq be preferable?
	 (*print-case* :downcase))
    (pprint (macroexpand-1 ',(ensure-unquoted expr)))))
