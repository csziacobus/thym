;;;; package.lisp

(defpackage #:thym
  (:use #:cl #:anaphora #:alexandria #:cl-ppcre #:cl-lex)
	(:shadow arg
					 exp
					 log
					 sin
					 cos
					 factorial
					 +
					 *
					 -
					 /
					 tan
					 sqrt
					 asin
					 acos
					 atan))
