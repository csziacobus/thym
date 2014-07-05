(in-package #:thym)

(defexpr ^ (expr) () (base exponent)
  (cond ((and (numberp base)
              (numberp exponent))
         (expt base exponent))
        ((eql base 0) 0)
        ((eql exponent 0) 1)
        ((eql exponent 1) base)
        ((eql base 1) 1)
        (t (make-expr '^ (list base exponent)))))

(defmethod print-object ((expr ^) stream)
  (format stream "~A ^ ~A" (base expr) (exponent expr)))

(defmacro power-bind ((base exponent) instance &body body)
  `(let ((,base (base ,instance))
         (,exponent (exponent ,instance)))
     ,@body))

(defmethod base ((expr ^))
  (first (args expr)))

(defmethod exponent ((expr ^))
  (second (args expr)))

(defmethod deriv ((expr ^) wrt &optional (n 1))
  (power-bind (base exponent) expr
    (+ (* expr (deriv exponent wrt n) (log base))
       (* (deriv base wrt n)
          exponent
          (^ base (+ exponent -1))))))

(defmethod equals ((x ^) y &key &allow-other-keys) nil)
(defmethod equals (x (y ^) &key &allow-other-keys) nil)

(defmethod equals ((x ^) (y ^) &key &allow-other-keys)
  (equal (args x) (args y)))
