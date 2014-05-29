;;;; thym.lisp

(in-package #:thym)

(defun op (expr) (first expr))
(defun args (expr) (rest expr))
(defun varp (obj) (symbolp obj))

(defun combine-constants (op args)
  (list* op
	 (apply op (mapcar #'sym (keep-numbers args)))
	 (mapcar #'sym (remove-numbers args))))

(defun sym (expr)
  "Simplifies on a prefix expression."
  (cond ((atom expr) expr)
	((some #'numberp (args expr))
	 (combine-constants (op expr) (args expr)))
	(t (cons (op expr) (mapcar #'sym (args expr))))))

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
