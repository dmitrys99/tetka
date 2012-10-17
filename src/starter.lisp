(in-package #:tetka)

(defun start ()
  (compile-templates)
  (init)
  (restas:start '#:restas.openid-auth :port +application-port+)
  (restas:start '#:tetka              :port +application-port+))

(defun init ()
  (init-logger)
  (init-db))

(export '(start init))
