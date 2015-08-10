(in-package #:symcl/core)

(defparameter +class-ordering+
  '(number
    symbol
    ^ * +
    derivative integral
    abs sign sqrt
    floor ceiling
    exp log
    sin cos tan cot asin acos atan acot
    sinh cosh tanh coth asinh acosh atanh acoth
    factorial
    function
    lambda))

(defun compare-classes (class other-class)
  (let* ((max-length (1+ (length +class-ordering+)))
         (index1 (or (position class +class-ordering+
                               :test #'subtypep)
                     max-length))
         (index2 (or (position other-class +class-ordering+
                               :test #'subtypep)
                     max-length)))
    (cond ((< index1 index2) '<)
          ((> index1 index2) '>)
          (t '=))))
