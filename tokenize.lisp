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

(defun tokenize (string)
  (do* ((token-generator (thym-tokens string))
	(token (multiple-value-list (funcall token-generator))
	       (multiple-value-list (funcall token-generator)))
	(value (first token) (first token))
	(tag (second token) (second token))
	prev-tag
	expr)
       ((not value)
	(read-from-string (format nil "~A" (nreverse expr))))
    (when (and (member prev-tag '(:number :var :rparen))
	     (member tag '(:op :var :lparen)))
      (push '* expr))
    (push value expr)
    (setf prev-tag tag)))

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
