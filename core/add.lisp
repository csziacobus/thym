(in-package #:symcl/core)

(defun +-sort! (args)
  (sort args #'sym<))

(define-associative-operator +
  :identity 0
  :combine-like-terms-by +-arg-simplify)

(defun +-arg-simplify (args)
  (with-like-terms (constant terms args +
                    :constant-fold cl:+
                    :partition multiplicative-partition
                    :identity 0)    
    (+-sort! (let ((collection (list constant)))
               (doalist (term coefficient terms collection)
                 (push (if (= coefficient 1)
                           term 
                           (* coefficient term))
                       collection))))))

(defmethod deriv ((expression +) wrt)
  (apply #'+ (mapcar (rcurry #'deriv wrt) (args expression))))


(defmethod additive-partition ((expression +))
  (values (first (args expression))
          (apply #'+ (rest (args expression)))))

(defun +-coefficient (expression)
  (values (additive-partition expression)))

(defun +-term (expression)
  (nth-value 1 (additive-partition expression)))
