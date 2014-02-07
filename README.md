REPL-UTILITIES 
==============

A set of utilities which ease life at the repl. 

REPL-UTILITIES is tested on SBCL, CCL and CLISP, and further porting should be
easy.

The Big Ideas
-------------

REPL-UTILITIES includes three sorts of features: __introspective__ procedures,
__miscellaneous utility__ functions, and, __pulling them together__, methods to
conveniently keep these symbols and optionally additional symbols available
in whichever package you switch to. 

For best effect load this package and use-package it from your lisp's init file.

If you wish to in-package another package at the repl in the course of
developing it, you can retain access to these REPL-UTILITIES by using DEV
to load and swap to the new package. DEV will import the REPL-UTILITIES
symbols into the new package, if they won't cause name conflicts. 

DEV also maps funcall over \*DEV-HOOKS\* after changing the package. \*DEV-HOOKS\*
is an empty list. I personally add hooks which import (via
REPL-UTILITIES:SHADOWED-IMPORT) several functions from other packages which
I always want available at the REPL, and to change the
\*DEFAULT-PATHNAME-DEFAULTS\* and emacs default-directory to match the source
location of the package just loaded.

My favorite operator in here is DEFLEX, taken from Rob Warnock and aliased
to LEX. It defines a global lexical variable -- this lets you use temporary
test variables without earmuffs safely:

    (defvar *x* (list 1 2 3)) 
    (mapcar #'print *x*) ; painful

    (lex x (list 1 2 3)) 
    (mapcar #'print x) ; better

Following the lead of CL:IN-PACKAGE, the package changing, loading,
and renaming operators (BRING, DEV, and NIC) expand into an EVAL-WHEN so
that they can take effect before later forms are read.

The symbol and package introspection operators have been defined as macros
to allow their arguments to be unquoted symbols. For convenience they
automatically unquote quoted arguments.

Features
========

The following is lightly edited output of
    (repl-utilities:summary repl-utilities)

Introspective Procedures
------------------------

    README:             Print the documentation on the exported symbols of a package.
    SUMMARY:            Print the exported symbols along with the first line of their docstrings.
    DOC:                Print any documentation for the symbol.
    DEPENDENCY-LOCATIONS:    Print the pathname of the system and of the systems needed to operation it.
    EXFNS:              Print the external fboundp symbols of a package.
    EXVS:               Print the external boundp symbols of a package.
    EXCS:               Print the external symbols for which find-class is truthy.
    EXTS:               Print the external symbols which are type specifiers.
    EXS:                Print the external symbols of package.
    NIC:                Add an additional nickname to package.
    ARGLIST:            Return the arglist for the given function name.
    DE:                 Shortening of describe. A Rob Warnock function.

Miscellaneous Utilities
-----------------------

    TRACE-PACKAGE:      Trace all of the symbols in *package*. 
    DEFLEX:             Define a top level (global) lexical VAR with initial value VAL,
    LEX:                Shortening of deflex: define a global lexical variable.
    PRINT-HASH:         Print the hash table as: Key, Value~% 
    MAC:                Bind *gensym-counter* to 0, Macroexpand-1 the form, pprint result.
    DBGV:               Print WHERE, execute FORMS, and print each form and its result to the STREAM.
    RIG:                Execute body with profiling and timing.
    REPEATABLY:         Use the same random state seed for every execution.

Pulling It Together
-------------------

    DEV:                Load package and IN-PACKAGE it. SHADOWED-IMPORT REPL-UTILITIES exported symbols.
    *DEV-HOOKS*:        List of functions to be funcalled after a package is loaded with DEV.
    BRING:              Load package and import its exported symbols.
    *BRING-HOOKS*:      List of functions to be funcalled after a package is loaded with BRING.
    SHADOWED-IMPORT:    Import each symbol into PACKAGE, unless a symbol of the same name is present.

To view full docstrings and argument lists type:
    (repl-utilities:readme repl-utilities)
in your the repl.

Examples of \*dev-hooks\*
=======================

One of my primary motivations for introducting \*dev-hooks\* was to
automate importing symbols that I always want available at the
repl. For example, if you want to keep my much-todo library at hand,
you can (from a context where it is already loaded) do the following:

    (defun todo-imports ()
      (repl-utilities:shadowed-import
        (loop for s being the external-symbols of :much-todo
              collect s)))

    (pushnew 'todo-imports *dev-hooks*)

The use of 'todo-imports instead of #'todo-imports is significant
for appropriate behavior when todo-imports is redefined.

This illustrates a reason I prefer importing to binding personal
functions to keywords even though importing leaves the possibility of
symbol conflicts: it encourages me to write code in a form that is
suitable for sharing as an ASDF system.

One hook I am quite fond of tries to sync the
\*default-pathname-defaults\* and emacs default-directory with the
package I am switching into.

    (defun d-p-d-package (&optional (package *package*))
      "If the package's name is a homonym for an asdf system, change the *d-p-d* to its
       location on disk and, if (setq slime-enable-evaluate-in-emacs t)
       in emacs, set the slime repl's pathname default as well."
       ;; slime-enable-evaluate-in-emacs warns that it can be a security risk
      (let ((pathloc (ignore-errors (asdf:component-pathname
                                     (asdf:find-system
                                      (intern (package-name package)
                                              :keyword))))))
        (cond (pathloc
               (setq *default-pathname-defaults* pathloc)
               (swank:eval-in-emacs
                `(with-current-buffer (slime-output-buffer)
                   (setq default-directory
                         ,(namestring *default-pathname-defaults*)))
                :nowait))
              (t (format t "~& Couldn't find a source location for ~A~%"
                           package)))))

    (pushnew 'd-p-d-package *dev-hooks*)

Installation
============

The most straitforward way to use REPL-UTILITIES, assuming you are
using quicklisp, is to place the following in your lisp's init file
after the quicklisp loading forms.

    (funcall (find-symbol (symbol-name '#:quickload) (symbol-name '#:ql))
             '#:repl-utilities)
    (use-package  '#:repl-utilities)

Or, in a running image, you can simply QL:QUICKLOAD it.

To load REPL-UTILITIES in an image without ASDF (an atypical use
case), you can load it with the following:

    (load "/path/to/repl-utilities/load")
    (use-package  '#:repl-utilities)

The REPL-UTILITIES features relating to systems wrap ASDF and
QUICKLISP functionality. When ASDF is unavailable they print a message
indicating the limitation.
