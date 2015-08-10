(in-package #:symcl/core)

(defun *-sort! (args)
  (sort args #'sym<))

(define-associative-operator *
  :identity 1
  :combine-like-terms-by *-arg-simplify)

(defun *-arg-simplify (args)
  (when (member 0 args)
    (return-from *-arg-simplify (list 0)))
  (with-like-terms (constant terms args *
                    :constant-fold cl:*
                    :partition (lambda (arg)
                                 (values (exponent arg)
                                         (base arg)))
                    :identity 1)
    (*-sort! (let ((collection (list constant)))
               (doalist (base exponent terms collection)
                 (push (^ base exponent) collection))))))

(defmethod multiplicative-partition ((expression *))
  (values (first (args expression))
          (apply #'* (rest (args expression)))))

(defun *-coefficient (expression)
  (values (multiplicative-partition expression)))

(defun *-term (expression)
  (nth-value 1 (multiplicative-partition expression)))
