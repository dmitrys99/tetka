(in-package #:tetka)


(restas:define-route main ("/")
  (tetka-templates:main))

;;; Отдаем статические файлы из каталога static
(restas:mount-submodule static (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("static"))
  (restas.directory-publisher:*directory* (path "static/"))
  (restas.directory-publisher:*default-render-method* *default-render-method*))

#|          |#
#| 404, GET |#
#|          |#
(restas:define-route not-found ("*any")
  (tetka-templates:page-404))

#|           |#
#| 404, POST |#
#|           |#
(restas:define-route not-found-post ("*any" :method :post)
  (tetka-templates:page-404))

