(in-package :cl-user)

(defpackage #:cl-uml-system
  (:use :cl :asdf))

(in-package :cl-uml-system)

(defsystem :cl-uml
  :version "0.2.0"
  :license "BSD"
  :author "Liutos"
  :components ((:file "package")
	       (:file "main" :depends-on ("package")))
  :description "A set of utilities written in Common Lisp for generating .dot file of UML class diagrams and relations.")