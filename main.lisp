(in-package :cl-uml)

(defclass class-diagram ()
  ((id :initarg :id)
   (name :initarg :name)
   (attribute :initarg :attribute
	      :initform "")
   (method :initarg :method
	   :initform "")))

(defgeneric gen-text (text))

(defmethod gen-text ((text string))
  "Format the string as one line text."
  (format nil "~A" text))

(defmethod gen-text ((text list))
  "Format the strings in the list TEXT as multiple line text."
  (format nil "~{~A~^<br/>~}" text))

(defun with-tr-td (content)
  "Enclose the CONTENT in tag tr and td."
  (format nil "<tr><td>~A</td></tr>" content))

(defun genlabel (name attr meth)
  "Generate the appropriate content of label according to NAME and ATTR and METH in dot language."
  (format nil "[shape = none, margin = 0, label = <<table border = \"0\" cellborder = \"1\" cellspacing = \"0\">~A~A~A</table>>]"
	  (with-tr-td name)
	  (with-tr-td (gen-text attr))
	  (with-tr-td (gen-text meth))))

(defun format-diagram (cls-diagram)
  "Generate the description of a diagram in dot language according to CLS-DIAGRAM."
  (with-slots (id name attribute method) cls-diagram
    (format t "~A ~A;~%" id (genlabel name attribute method))))

(defmacro with-uml (filespec &body body)
  "Generate the essential elements for drawing diagraph and redirect the output of BODY to the file specified by FILESPEC."
  `(with-open-file (*standard-output* ,filespec
				      :direction :output
				      :if-exists :supersede)
     (format t "digraph G {~%")
     ,@body
     (format t "}")))

(defun genbinding (comp-list)
  "Generate the list to be used in the `bindings' list of special-form `let'."
  (destructuring-bind (id name attr meth) comp-list
    `(,id (make-instance 'class-diagram
			 :id ,(format nil "~(~A~)" id)
			 :name ,name
			 :attribute ,attr
			 :method ,meth))))

(defmacro clet (bindings &body body)
  "To bind the symbol and the instance of class class-diagram conveniently."
  `(let ,(mapcar #'genbinding bindings)
     ,@body))

(defun format-rel (cls-src cls-dst relspec)
  "Using the relation RELSPEC to connect the CLS-SRC to CLS-DST."
  (format t "~A -> ~A ~A;~%"
	  (slot-value cls-src 'id)
	  (slot-value cls-dst 'id)
	  relspec))

(defun format-use-rel (cls1 cls2)
  "CLS1 use CLS2."
  (format-rel cls1 cls2 "[style = dashed]"))

(defun format-converge-rel (cls1 cls2)
  "CLS2 is part of CLS1."
  (format-rel cls1 cls2 "[arrowhead = \"odiamond\"]"))

(defun format-implement-rel (cls1 cls2)
  "CLS1 implements the interface described by CLS2."
  (format-rel cls1 cls2 "[style = dashed, arrowhead = empty]"))

(defun format-inheritance-rel (cls1 cls2)
  "CLS1 inherit CLS2."
  (format-rel cls1 cls2 "[arrowhead = empty]"))