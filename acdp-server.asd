;;;; acdp-app.asd

(asdf:defsystem #:acdp-server
  :serial t
  :description "A web interface to acdp, the alsa cd player."
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT"
  :depends-on (#:acdp #:cl-json #:ningle #:spinneret
		      #:parenscript #:trivial-dump-core)
  :components ((:file "package-server")
               (:file "acdp-server")))
