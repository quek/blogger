;;;; -*- Mode: LISP; -*-
(asdf:defsystem :blogger
  :version "0.0.0"
  :serial t
  :components ((:file "packages")
               (:file "blogger"))
  :depends-on (drakma cl-ppcre s-xml cl-unicode))
