(defsystem tetka
  :depends-on (#:hunchentoot
	       #:restas
	       #:restas-openid-auth
               #:restas-directory-publisher
	       #:closure-template
	       #:cl-openid
	       #:iterate
	       #:log4cl
	       ;#:chillax
	       #:moptilities
	       #:local-time
	       #:puri
	       #:flexi-streams
	       ;#:cxml
	       ;#:xpath
	       #:external-program
	       #:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "config" )
               (:module "src" :serial t
                :components ((:file "templates")
                             (:file "routes"   )
                             (:file "starter"  )))))


