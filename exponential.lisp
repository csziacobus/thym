(in-package #:thym)

(defexpr exp-base (efun) () ())

(defmethod inverse ((fun exp-base))
  'logarithm)

(defmethod exponent ((expr exp-base))
  (first (args expr)))

(defmethod base ((expr exp-base)) e)

(defexpr exp (exp-base) () (arg)
  (case arg
    (0 1)
    (1 e)
    (otherwise (make-expr 'exp (list arg)))))

(defmethod print-object ((object exp) stream)
  (format stream
          "e ^ ~A"
          (list-if-precedence '^ (exponent object))))

(defmethod antideriv ((expr exp))
  (lambda (u) (exp u)))

(defmethod first-deriv ((expr exp) wrt)	expr)

(defexpr log (efun) () (arg)
  (make-expr 'log (list arg)))

(defmethod first-deriv ((expr log) wrt)
  (^ (first (args expr)) -1))

(defmethod inverse ((expr log))
  'exp)

(defmethod antideriv ((expr log))
  (lambda (u) (* u (log u))))
