;;;; pretty.lisp

(in-package #:thym)

(defun ssym (string)
  "Takes infix string, simplifies, spits out a nice output string."
  (funcall (compose #'untokenize
		    #'unparenthesize
		    #'prefix->infix
		    #'sym
		    #'infix->prefix
		    #'parenthesize
		    #'tokenize)
	   string))
