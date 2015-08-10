(in-package #:symcl/functions)

(defclass arbitrary-function (expression) ())
(defclass elementary-function (expression) ())

(defun negative-coefficient-p (expression)
  (and (typep expression '^)
       (let ((coefficient (*-coefficient expression)))
         (minusp coefficient))))

(defgeneric function-arity (function))
(defgeneric first-deriv (function))
