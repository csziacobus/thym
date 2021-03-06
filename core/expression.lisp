(in-package #:symcl/core)

(defclass expression ()
  ((%args :accessor args :initarg :args)))

(defun expressionp (obj)
  (typep obj 'expression))

(defun copy-expression (expression)
  (check-type expression expression)
  (make-instance (class-of expression) :args (args expression)))

(defmacro with-coefficient-term ((coefficient term) expression
                                 &body body)
  `(multiple-value-bind (,coefficient ,term)
       (multiplicative-partition ,expression)
     ,@body))

(defun compare (x y)
  (if (eq x y)
      '=
      (let ((class-comparison (compare-classes (type-of x)
                                               (type-of y))))
        (if (eq class-comparison '=)
            (multiple-value-bind (x y)
                (if (and (numberp x) (numberp y))
                    (values x y)
                    (values (sxhash x) (sxhash y)))
              (when (and (numberp x) (numberp y))
                (cond ((< x y) '<)
                      ((> x y) '>)
                      (t '=))))
            class-comparison))))

(macrolet ((def (fun op)
             `(defun ,fun (expression other-expression)
                (eq (compare expression other-expression) ',op)))
           (defx= (fun op)
             `(defun ,fun (expression other-expression)
                (member (compare expression other-expression)
                        '(,op =)))))
  (def sym< <)
  (def sym> >)
  (def sym= =)
  (defx= sym>= >)
  (defx= sym<= <))

(defun expression= (expression other-expression)
  (or (eql expression other-expression)
      (and (expressionp expression) (expressionp other-expression)
           (eq (class-of expression) (class-of other-expression))
           (equal (args expression) (args other-expression)))))

(macrolet ((def (fun default-value docstring)
             `(defgeneric ,fun (expression)
                (:documentation ,docstring)
                (:method ((expression t))
                  ,default-value))))
  (def base expression "Return the base of the expression.")
  (def exponent 1 "Return the exponent of the expression."))

(macrolet ((def (fun identity docstring)
             `(defgeneric ,fun (expression)
                (:documentation ,docstring)
                (:method (expression)
                  (values ,identity expression)))))
  (def multiplicative-partition 1 "Return coefficient and term.")
  (def additive-partition 0 "Return constant and rest of expression."))

(defgeneric deriv (expression wrt))
(defgeneric integrate (expression limits))
