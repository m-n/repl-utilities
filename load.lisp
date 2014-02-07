;;;; load.lisp

;; To load repl-utilities into an image without asdf (an atypical use
;; case), load this file.

;; If you are using asdf or quicklisp (as typical) to load
;; repl-utilities, this file will not be loaded and can be safely
;; ignored.

(in-package #:cl-user)

(let ((path #.(or *compile-file-truename*
                  *load-truename*
                  *default-pathname-defaults*)))
  (flet ((compile-load (file)
           (load (compile-file
                  (make-pathname :name file :type "lisp" :defaults path)
                  :print ()
                  :verbose ())
                 :print ()
                 :verbose ())))
    (when *load-verbose*
      (princ "; Loading REPL-UTILITIES" *standard-output*))
    (mapc #'compile-load '("package" "utils" "repl-utilities"))))
