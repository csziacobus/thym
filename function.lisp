(in-package #:thym)

(defexpr fun (expr) () ())

(defmethod print-object ((expr fun) stream)
  (let ((args (args expr)))
    (format stream
            "~A(~A~{, ~A~})"
            (type-of expr) (first args) (rest args))))

(defgeneric with-respect-p (fun)
  (:documentation
   "Allow derivatives with respect to functions."))

(defgeneric first-deriv (fun wrt)
  (:documentation
   "Returns the first derivative of the function."))


(defgeneric antideriv (expr)
  (:documentation
   "Looks up the antiderivative of a function and returns a lambda expression."))

(defgeneric inverse (fun)
  (:documentation
   "Returns the inverse of a function as a class designator."))

(defgeneric arg (fun)
  (:documentation
   "Returns the argument of a unary function.")
  (:method ((expr fun))
    (first (args expr))))

(defmethod deriv ((expr fun) wrt &optional (n 1))
  "Derivative for arbitrary functions using chain rule."
                                        ; (deriv (f x) wrt) -> (deriv x wrt) * (first-deriv f wrt)
                                        ; FIXME assumes 1 arg chain rule
  (if (zerop n)
      expr
      (let ((next-deriv (* (deriv (arg expr) wrt) 
                           (first-deriv expr wrt))))
        (dotimes (i (1- n) next-deriv)
          (setf next-deriv (deriv next-deriv wrt))))))

(defmethod equals ((x fun) (y fun) &key) ; one arg-assumption
  (or (eq x y) (and (eq (type-of x) (type-of y))
                    (equals (arg x) (arg y)))))

(defexpr efun (fun) () ())
