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
        ((eq (first expr) '-) expr) ; Unary minus`
        ((member-if #'infix-p expr)
         (let ((pos (find-max-precedence expr)))
           (list (parenthesize (subseq expr 0 pos))
                 (nth pos expr)
                 (parenthesize (subseq expr (1+ pos))))))
        (t expr)))

(defun infix->prefix (expr)
  "Converts a fully parenthesized expression into prefix, flattening associative operators and replacing minuses."
  (cond ((or (atom expr) (eq (first expr) 'quote)) expr)
        ((singlep expr)
         (infix->prefix (first expr)))
        ((not (infix-p (second expr)))
         (destructuring-bind (op . args) expr
           (let ((args (infix->prefix args)))
             (list* op (if (eq (first args) 'quote)
                           (list args)
                           (ensure-list args))))))
        (t
         (destructuring-bind (lhs op rhs) expr
           (let ((lhs (infix->prefix lhs))
                 (rhs (infix->prefix rhs)))
             (list op lhs rhs))))))

(defun intersperse-with-precedence (expr)
  (let ((op (type-of expr)))
    (labels ((make-list-with-precedence (expr)
               (if (or (numberp arg)
                       (symbolp arg)
                       (> (or (precedence (type-of arg)) 0)
                          (or (precedence op))))
                   arg
                   (list arg))))
      (rest (loop for arg in (make-list-with-precedence args)
               collect op collect arg)))))

(defmacro parse (string)
  (funcall (compose #'infix->prefix
                    #'parenthesize
                    #'tokenize)
           string))
