(in-package #:tetka)

(defun start-tetka ()
  (restas:start '#:restas.openid-auth :port +application-port+)
  (restas:start '#:tetka              :port +application-port+))
  
(export 'start-tetka)