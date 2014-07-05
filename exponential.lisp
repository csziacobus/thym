(in-package #:thym)

(defexpr exp-base (fun) () ())

(defmethod inverse ((fun exp-base))
	'logarithm)

(defmethod exponent ((expr exp-base))
	(first (args expr)))

(defexpr exp (exp-base) () (arg)
	(make-expr 'exp (list arg)))

(defmethod base ((expr exp))
	(exp 1))

(defmethod first-deriv ((expr exp) wrt)	expr)

(defexpr log (fun) () (arg)
	(make-expr 'log (list arg)))

(defmethod first-deriv ((expr log) wrt)
	(^ (first (args expr)) -1))

(defmethod inverse ((expr log))
	'exp)
