(in-package #:tetka)

(defun compile-templates ()
  (closure-template:compile-template :common-lisp-backend (path "templates/404.soy"))
  (closure-template:compile-template :common-lisp-backend (path "templates/main.soy")))

(compile-templates)
