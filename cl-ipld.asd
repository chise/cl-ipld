(defsystem :cl-ipld
  :description "A Common Lisp based implementation of IPLD"
  :version "0.0"
  :author "Tomohiko Morioka"
  :licence "LGPL"
  :depends-on (:ironclad :cbor :cl-base32)
  :serial t
  :components ((:file "package")
	       (:file "cid")))
