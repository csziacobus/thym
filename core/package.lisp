(in-package #:symcl)

(alexandria:define-constant +symcl-common-lisp-shadowed-symbols+
    '(:shadow
      + - * / sqrt
      exp log
      sin cos tan
      asin acos atan
      sinh cosh tanh
      asinh acosh atanh
      pi)
  :test #'equal)

(defpackage #:symcl/core
  (:use #:common-lisp #:alexandria #:iterate)
  #.symcl::+symcl-common-lisp-shadowed-symbols+
  (:export #:*-coefficient
           #:+-coefficient
           #:args
           #:deriv
           #:integrate
           #:expressionp
           #:expression
           #:copy-expression
           #:with-coefficient-term
           #:multiplicative-partition
           #:additive-partition
           #:base
           #:exponent
           #:^))
