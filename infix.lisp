;;;; infix.lisp

(in-package #:thym)

(defmacro define-infix (op precedence &key (assoc :left))
  `(add-op ',op ,precedence :assoc ,assoc))

(let (infix-ops)
  (defun infix-props (op) (assoc op infix-ops))
  
  (defun add-op (op precedence &key assoc)
    (pushnew `(,op ,precedence ,assoc) infix-ops :test #'equal)))

(define-infix = 1)
(define-infix + 2)
(define-infix - 2)
(define-infix * 3)
(define-infix / 3)
(define-infix ^ 4 :assoc :right)

(defun infix-p (op) (infix-props op))
(defun precedence (op) (second (infix-props op)))
(defun left-assoc (op) (eq (third (infix-props op)) :left))
(defun right-assoc (op) (eq (third (infix-props op)) :right))

(defun max-precedence (ops)
  "Returns the operator with the lowest precedence."
  (first (stable-sort (reverse ops) #'< :key #'precedence)))

(defun all-ops (expr)
  "Return all operators of an expression. Preserves order."
  (remove-if (complement #'infix-p) expr))

(defun find-max-precedence (expr) ; UGLY, use find instead?
  "Finds the position of the operator with highest precedence."
  (let ((op (max-precedence (all-ops expr))))
    (position op expr :from-end (left-assoc op))))

(defun parenthesize (expr)
  "Fully parenthesizes an infix expression."
  (cond ((atom expr) expr)
	((singlep expr) (parenthesize (first expr)))
	((eq (first expr) '-) expr) ; Unary minus
	((member-if #'infix-p expr)
	 (let ((pos (find-max-precedence expr)))
	   (list (parenthesize (subseq expr 0 pos))
		 (nth pos expr)
		 (parenthesize (subseq expr (1+ pos))))))
	(t expr)))

(defun unparenthesize (expr)
  "Fully unparenthesize an infix expression based on precedence."
  (if (or (atom expr) (not (member-if #'infix-p expr)))
      expr
      (destructuring-bind (lhs op rhs) expr
	(labels ((make-list-by-precedence (expr)
		   (if (or (atom expr)
			  (< (or (precedence (second expr)) 0)
			     (precedence op)))
		       (list (unparenthesize expr))
		       (unparenthesize expr))))
	  (append (make-list-by-precedence lhs)
		  (list op)
		  (make-list-by-precedence rhs))))))

(defun infix->prefix (expr)
  "Converts a fully parenthesized expression into prefix, flattening associative operators and replacing minuses."
  (cond ((atom expr) expr)
	((singlep expr)
	 (infix->prefix (first expr)))
	((eq (first expr) '-) ;; Unary minus
	 (destructuring-bind (op . args) expr
	   (declare (ignore op))
	   `(* -1 ,(infix->prefix args))))
	((not (member-if #'infix-p expr))
	 (destructuring-bind (op . args) expr
	   (let ((args (infix->prefix args)))
	     (list* op (ensure-list args)))))
	(t
	 (destructuring-bind (lhs op rhs) expr
	   (let ((lhs (infix->prefix lhs))
		 (rhs (infix->prefix rhs)))
	     (if (eq op '-)
		 `(+ ,lhs (* -1 ,rhs))
		 (labels ((level (expr)
			    (if (and (member op '(+ *))
				   (consp expr)
				   (eq op (first expr)))
				(rest expr)
				(list expr))))
		   `(,op ,@(level lhs) ,@(level rhs)))))))))

(defun intersperse (op args)
  (if (singlep args)
      (first args)
      (list (first args)
	    op
	    (intersperse op (rest args)))))

(defun prefix->infix (expr)
  "Converts a prefix expression into infix for consumption."
  (if (or (atom expr) (not (infix-p (first expr))))
      expr
      (intersperse (first expr)
		   (mapcar #'prefix->infix (rest expr)))))

(defun level (expr)
  (infix->prefix (prefix->infix expr)))
