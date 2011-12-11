(with-uml "4p4.dot"
  (clet ((foo
	  "RollingDice"
	  ""
	  "main (args : String [] ) : void")
	 (bar
	  "Die"
	  "faceValue : int"
	  '("roll() : int"
	    "setFaceValue(int value) : void"
	    "getFaceValue() : int"
	    "toString() : String")))
    (format-diagram foo)
    (format-diagram bar)
    (format-use-rel foo bar)))