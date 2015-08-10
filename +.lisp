(in-package #:thym)

(defmethod deriv ((expr +) wrt &optional (n 1))
  (apply #'+ (mapcar (rcurry #'deriv wrt) (args expr))))

(defmethod collect-like-terms ((expr +))
  (labels ((recur (args terms constant &aux (arg (first args)))
             (typecase arg
               (null (values terms constant))
               (number (recur (rest args)
                              terms
                              (cl:+ constant arg)))
               (+ (recur (append (rest args) (args arg))
                         terms
                         constant))
               (otherwise
                (with-coeff-term (coeff term) arg
                  (recur (rest args)
                         (let ((val (assoc term terms
                                           :test #'equals)))
                           (if val
                               (progn (incf (cdr val) coeff)
                                      terms)
                               (acons term coeff terms)))
                         constant))))))
    (multiple-value-bind (terms constant)
        (recur (args expr) () 0)
      (string-sort
       (cons constant (iter (for (term . coeff) in terms)
                        (cond ((zero? coeff))
                              ((eql coeff 1) (collect term))
                              ((typep term '*)
                               (collect (apply #'* coeff (args term))))
                              (t (collect (* coeff term))))))))))
