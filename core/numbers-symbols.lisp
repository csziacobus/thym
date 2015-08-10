(in-package #:symcl/core)

(defmethod deriv ((expression number) (wrt t)) 0)
(defmethod deriv ((expression symbol) wrt)
  (if (eq expression wrt) 1 0))
