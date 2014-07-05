(in-package #:thym)

(defexpr combinatorial-function (fun) () ())

(defexpr factorial (combinatorial-function) () (arg)
	(if (numberp arg)
			(alexandria:factorial arg)
			(make-expr 'factorial (list arg))))
