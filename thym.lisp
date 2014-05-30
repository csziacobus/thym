;;;; thym.lisp

(in-package #:thym)

(defun ^ (base power) (expt base power))

(defun op (expr) (first expr))
(defun args (expr) (rest expr))
(defun varp (obj) (symbolp obj))

(defun operatorp (op expr) (eq (op expr) op))
(defun sump (expr) (operatorp '+ expr))
(defun productp (expr) (operatorp '* expr))
(defun powerp (expr) (operatorp '^ expr))

(defun sym-op (op) (symbolicate "s" op))

(defmacro defsym (op params &body body)
  `(defun ,(sym-op op) ,params
     ,@body))

(defun combine-constants (op args)
  (list* op
	 (apply op (mapcar #'sym (keep-numbers args)))
	 (mapcar #'sym (remove-numbers args))))

(defsym + (&rest args)
  (if (some #'numberp args)
      (combine-constants '+ args)
      `(+ ,@args)))

(defsym * (&rest args)
  (if (some #'numberp args)
      (combine-constants '* args)
      `(* ,@args)))

(defun sym (expr)
  "Simplifies on a prefix expression."
  (if (atom expr)
      expr
      (if (fboundp (sym-op (op expr))) ;; Check if there are rewrite rules available
	  (apply (sym-op (op expr)) (mapcar #'sym (args expr)))
	  (list* (op expr) (mapcar #'sym (args expr))))))

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
