;;;; acdp.asd

(asdf:defsystem #:acdp
  :serial t
  :description "A lisp interface to acdp, the alsa cd player."
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT"
  :depends-on (#:external-program #:cl-json #:bordeaux-threads)
  :components ((:file "package")
               (:file "acdp")))
