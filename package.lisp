(in-package :cl-uml-system)

(defpackage #:cl-uml
  (:use :cl)
  (:export :class-diagram
	   :format-diagram
	   :with-uml
	   :clet
	   :format-use-rel
	   :format-converge-rel
	   :format-implement-rel
	   :format-inheritance-rel))

(in-package :cl-uml)