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
