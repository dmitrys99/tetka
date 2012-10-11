(restas:define-module #:tetka
  (:use :cl))
  
(in-package #:tetka)

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system (intern (package-name *package*)))))

(defun path (relative)
  (merge-pathnames relative *basedir*))