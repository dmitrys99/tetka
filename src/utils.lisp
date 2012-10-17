(in-package #:tetka)

;;; =========== Logging ================================================

(defun init-logger ()

  (setf *logger* (log:make-logger))

  (log:config *logger*
	      :immediate-flush t
	      :sane
	      :daily   +log-file+
	      :pattern "%D{%Y-%m-%d %H:%M:%S} [%p] %t %m%n")

  (info-message "Запустила протоколирование"))


;;;; TODO DRY!
(defun debug-message (message)
  (log:debug *logger* (untilde message)))

(defun error-message (message)
  (log:error *logger* (untilde message)))

(defun info-message (message)
  (log:info *logger* (untilde message)))

(defun warning-message (message)
  (log:warn *logger* (untilde message)))

;;; =========== Date & Time ============================================

(defun now ()
"Функция now

Возвращает текущее время в виде списка

Входные данные:
  нет

Выходные данные
   timestamp"
  (local-time:now))

(defun past ()
  "Функция past

Возвращает время заведомо в прошлом.

Входные данные:
  нет

Выходные данные:
  timestamp"
  (local-time:timestamp+ (local-time:now) -50 :year))

(defun ts-to-string (ts)
  (local-time:format-timestring nil ts))

(defun string-to-ts (str)
  (local-time:parse-timestring str :fail-on-error nil))

(defun format-time (str-time)
"Функция возвращает дату в формате, пригодном для чтения человеком"
  (let ((time (list-to-ts str-time)))
    (when time
      (local-time:format-timestring nil
				    time
				    :format '((:DAY 2) #\. (:MONTH 2) #\. (:YEAR 4) " " (:HOUR 2) #\: (:MIN 2))))))



;;; =========== Идентификаторы =========================================

(defun uuid (&optional (server *db-server*))

"Функция uuid
Возвращает уникальный идентификатор с сервера CouchDB

Входные данные:
  нет

Выходные данные
  строка с уникальным идентификатором"

  (multiple-value-bind
	(response status)
      (chillax:get-uuids server :number 1)
    (case status
      (:ok (cadadr response))
      (otherwise nil))))

;;; =========== Пароли =================================================

(defun calc-hash (pass salt)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
			     (ironclad:ascii-string-to-byte-array
			      (concatenate 'string  pass salt)))))

(defun random-passwd (len)
  "Генератор паролей/соли"
  (let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()")
	 (n (length chars)))
    (coerce
     (loop for i from 1 to len
	   collect (elt chars (random n)))
     'string)))

; ============== Идентификаторы ======================================

(defstruct counter
  (counter 0 :type (unsigned-byte
  #+x86    32
  #+x86-64 64
  )))

(defvar *product-counter* (make-counter)
  "Счетчик продуктов")

(defvar *order-counter* (make-counter)
  "Счетчик заказов")

(defun init-product-id ()
  (if (not (zerop (counter-counter *product-counter*)))
      :already-inited
      (let ((rows (first
		   (jsown:val (chillax:query-view *products-db*
						  "locate"
						  "by-product-id"
						  :limit 1
						  :descendingp t) "rows"))))
	(sb-ext:atomic-incf (counter-counter *product-counter*)
			    (if rows
				(jsown:val rows "key")
				*start-product-id*))
	:inited)))

(defun init-order-id ()
  (if (not (zerop (counter-counter *order-counter*)))
      :already-inited
      (let ((inc nil)
	    (rows (first
		   (jsown:val (chillax:get-document *orders-db*
						    "_all_docs"
						    :params (list (cons "descending" "true")
								  (cons "limit"         "1")
								  (cons "startkey"  "\"9\"")
								  (cons "endkey"    "\"0\"")))
			      "rows"))))

	(setf inc
	      (if rows
		  (handler-case
		      (parse-integer (jsown:val rows "key") :junk-allowed nil)
		    (error (c) (declare (ignore c))))

		  *start-order-id*))
	#|                                                              |#
	#| Этот кусок нужен, если последний номер заказа в БД - не число |#
	#|                                                              |#
	(if (not inc)
	    (setf inc *start-order-id*))


	(sb-ext:atomic-incf (counter-counter *order-counter*) (1+ inc))

	:inited)))



(defun get-next-product-id ()
  (sb-ext:atomic-incf (counter-counter *product-counter*)))

(defun get-next-order-id ()
  (sb-ext:atomic-incf (counter-counter *order-counter*)))

(defun plist-hash-table (plist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (do ((tail plist (cddr tail)))
        ((not tail))
      (setf (gethash (car tail) table) (cadr tail)))
    table))

(defparameter *char-replacements*
  (plist-hash-table
   (list
    #\Backspace (code-char 32)
    #\Vt        (code-char 32))))


(defun strip (string)
  (dotimes (i (length string))
    (let* ((char (aref string i))
           (replacement (gethash char *char-replacements*)))
      (if replacement
          (setf (aref string i) replacement))))
  string)


(defun log-fetch (authority status &optional (time (now)))
  (chillax:put-document *fetch-db*
			(concatenate 'string status "$" authority "$" (local-time:format-timestring nil time))
			(list :OBJ
			      (cons "ID" authority)
			      (cons "STATUS" status))))

(defun file-size (filename)
  (with-open-file (s filename)
    (file-length s)))

(defun encode-string (str not-encode)
  (if (stringp str)
      (with-output-to-string (out)
        (iter (for ch in-string str)
              (if (or (char<= #\0 ch #\9) (char<= #\a ch #\z) (char<= #\A ch #\Z)
                      (find ch not-encode :test #'char=))
                  (write-char ch out)
		  (if (char= ch #\ )
		      (format out "+")
		      (iter (for octet in-vector (babel:string-to-octets (string ch) :encoding :utf-8))
			    (format out "%~2,'0x" octet))))))
      str))


(defun encode-uri (str)
  (encode-string str "~!#$*()=:,;?+'"))


(defun scroller (page count/page count text &optional (max-pages 11))
  "Функция генерирует скроллер: < 1 2 3 4 >"
  (let* ((need-left-arrow (not (zerop page)))
         (pages (ceiling (/ count count/page)))
         (need-right-arrow (not (= page (1- pages))))
         (max-pages/2 (ceiling max-pages 2))
         (start 0)
	 (txt (encode-uri text))
         (finish 0)
	 (s (make-string-output-stream))
	 (res ()))
;    (break "~A" txt)
    (if  (or (< page 0) (<= pages page))
        (return-from scroller :invalid-page-number))

    (cond
      ((< page max-pages/2)           (setf start 0
                                            finish (1- (min max-pages pages))))
      ((> page (- pages max-pages/2)) (setf start (- pages max-pages)
                                            finish (1- pages)))
      (T                              (setf start (- page max-pages/2 -1)
                                            finish (+ page (1- max-pages/2)))))

    (let ((start (max (1+ start) 0))
          (finish (1+ finish))
          (page (1+ page)))
      ;(break "~A" start)
      (if need-left-arrow (setf res (cons (list :text txt :number "<" :page (- page 2)) res)))
      (if need-left-arrow (princ "L " s))

      (loop for i from start to finish
	 if (= page i)
	 do (setf res (cons (list :text txt :number i :page (1- i) :self T) res))
	 else
	 do (setf res (cons (list :text txt :number i :page (1- i)) res))
	 do (princ (if (= page i) "*" i) s)
	 do (princ " " s))

      (if need-right-arrow (princ " R" s))
      (if need-right-arrow (setf res (cons (list :text txt :number ">" :page page) res))))

    (values (reverse res) (get-output-stream-string s))))


(defun get-dbs-info ()
  (let ((res  ())
	(db   ())
	(info ())
	(dbs (remove-if #'(lambda (x)
			    (eql (elt x 0) #\_)) ; exclude system DBs.
			(chillax:all-dbs *db-server*))))

;    (break "~A" DBS)
    (setf res
	  (append
	   (list :dbinfos
		 (loop for d in dbs
		    do (setf db (chillax:ensure-db *db-server* d))
		    do (setf info (chillax:db-info db))
					;do (break "~A" info)
		    collect (loop for s in '("db_name" "doc_count" "disk_size")
			       collect (intern (string-upcase s) 'keyword)
			       collect (jsown:val info s))))))
    res))


(defun untilde (s)
  (if (stringp s)
      (multiple-value-bind (a b)
	  (cl-ppcre:regex-replace-all "~" s "~1~")
	(declare (ignore b))a)
      nil))

(defun split (string &optional &key max (ws '(#\space #\tab #\- #\\ #\/ #\& #\@ #\$ #\% #\!)))
  (remove-if #'(lambda (x) (string= "" x))
	     (flet ((is-ws (char) (find char ws)))
	       (nreverse
		(let ((list nil)(start 0)(words 0) end)
		  (loop
		     (when (and max (>= words (1- max)))
		       (return (cons (subseq string start) list)))

		     (setf end (position-if #'is-ws string :start start))
		     (push (subseq string start end) list)
		     (incf words)
		     (unless end (return list))
		     (setf start (1+ end))))))))

(defun join (delim args)
  (labels ((join1 (so-far delim args)
	     (if args
		 (join1 (concatenate 'string so-far delim
				     (princ-to-string (car args)))
			delim (cdr args))
		 so-far)))
    (join1 (car args) delim (cdr args))))
