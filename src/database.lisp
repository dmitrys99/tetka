#|===========================================================================

Модуль отвечает за работу с БД:
содержит переменные-ссылки на БД и сервер, а также процедуру инициализации.

===========================================================================|#

(in-package #:tetka)

;;; Ссылка на сервер CouchDB
(defvar *db-server* nil)

;;; Список БД
(defvar *users-db* ())
(defvar *products-db* ())
(defvar *orders-db* ())

(defvar *users-db-name* "users-db")
(defvar *products-db-name* "products-db")
(defvar *orders-db-name* "orders-db")


(defmacro db-connect (db-var db-name)
  `(progn
     (handler-case
	 (setf ,db-var (chillax:ensure-db *db-server* ,db-name))
       (error (c)
	 (declare (ignore c))
	 (error-message (format nil "Не могу подключиться к БД ~A (Сервер работает?)~%" ,db-name)))
       (:no-error (c)
	 (declare (ignore c))
	 (info-message (format nil "Подключилась к БД ~A." ,db-name))))))

(defun init-db ()
  (info-message "Настраиваю подключения к базам данных")
  (setf *db-server* (make-instance 'chillax:jsown-server))
  (if (not *db-server*)
      (error-message "!!Не могу создать сервер JSOWN.")
      (info-message ":: Создала экземпляр сервера JSOWN."))

  (db-connect *users-db* *users-db-name*)
  (db-connect *products-db* *products-db-name*)
  (db-connect *orders-db* *orders-db-name*)

  (info-message ":: Проверяю представления")
  (check-views)

  (info-message ":: Настраиваю номера продуктов")
  (init-product-id)
  
  (info-message ":: Настраиваю номера заказов")
  (init-order-id)

  (if (and
       *db-server*
       *users-db*
       *products-db*
       *orders-db*) *db-server* nil))

(defun check-views ()
  #| ====== GOODS-DB ====== |#
  (info-message ":::: Проверяю представления для PRODUCTS-DB")
  (if (null (chillax:get-document-revision *products-db* "_design/locate" :errorp nil))
      (chillax:put-document *products-db*
			    "_design/locate"
			    '(:OBJ  ("views" :OBJ
				     ("by-product-id" :OBJ
				      ("map" . "function (doc) {if (doc.product_id != undefined) {emit(doc.product_id, 1)}}")))))))

