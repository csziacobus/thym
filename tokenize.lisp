;;;; tokenize.lisp

(in-package #:thym)

(define-string-lexer thym-tokens
  ("\\(" (return (values (gensym) :lparen)))
  ("\\)" (return (values (gensym) :rparen)))
  ("\\[" (return (values (gensym) :lparen)))
  ("\\]" (return (values (gensym) :rparen)))
  ("\\{" (return (values (gensym) :lparen)))
  ("\\}" (return (values (gensym) :rparen)))
  ("\\+" (return '+))
  ("\\-" (return '-))
  ("\\*" (return '*))
  ("/" (return '/))
  ("\\^" (return '^))
  ("\\i" (return #C (0 1)))
  ("pi" (return 'pi))
  ("π" (return 'pi))
  ("e" (return '(exp 1)))
  ("sin" (return (values 'sin :op)))
  ("cos" (return (values 'cos :op)))
  ("tan" (return (values 'tan :op)))
  ("asin" (return (values 'asin :op)))
  ("acos" (return (values 'acos :op)))
  ("atan" (return (values 'atan :op)))
  ("sinh" (return (values 'sinh :op)))
  ("cosh" (return (values 'cosh :op)))
  ("tanh" (return (values 'tanh :op)))
  ("asinh" (return (values 'asinh :op)))
  ("acosh" (return (values 'acosh :op)))
  ("atanh" (return (values 'atanh :op)))
  ("log" (return (values 'log :op)))
  ("ln" (return (values 'log :op)))
  ("[A-Za-z_]" (return (values (read-from-string $@) :var)))
  ("[0-9]*\\.?[0-9]+" (return (values (read-from-string $@) :number)))
  ("[ \\n\\t]+" #| ignore whitespace |#))

(defparameter +delimiters+
  (list :lparen :rparen))

(defun tokenize (string)
  (let ((token-generator (thym-tokens string))
	prev-tag (expr (list nil)))
    (loop
       (multiple-value-bind (value tag) (funcall token-generator)
	 (when (not value)
	   (return))
	 (if (member tag +delimiters+)
	     (if (eq tag :lparen)
		 (progn
		   (when (member prev-tag '(:number :rparen))
		     (setf (first expr)
			   (append (first expr)
				   (list '*))))
		   (push () expr))
		 (let ((our-context (pop expr)))
		   (setf (first expr)
			 (append (first expr)
				 (list our-context)))))
	     (progn
	       (when (and (member prev-tag '(:number :var :rparen))
			(member tag '(:op :var :lparen)))
		 (setf (first expr)
		       (append (first expr) (list '*))))
	       (setf (first expr)
		     (append (first expr)
			     (list value)))))
	 (setf prev-tag tag)))
    (car (nreverse expr))))

(defun untokenize (expr)
  "Takes a fully parenthesized list."
  (if (atom expr)
      (write-to-string expr)
      (with-output-to-string (string)
	(dolist (element expr)
	  (when (consp element)
	    (format string "("))
	  (format string "~(~a~)" (untokenize element))
	  (when (consp element)
	    (format string ")"))))))
