(in-package #:thym)

(defexpr + (assoc-expr) ((identity :allocation :class
                                   :initform 0))
    (&rest args)
  (assoc-expr '+ args))

(defmethod deriv ((expr +) wrt &optional (n 1))
  (apply #'+ (mapcar (rcurry #'deriv wrt n) (args expr))))

(defmethod level ((expr +))
  (labels ((iter (args terms coeff &aux (arg (first args)))
             (cond
               ((null args) (values coeff terms))
               ((numberp arg)
                (iter (rest args) terms (cl:+ coeff arg)))
               ((typep arg '+)
                (iter (append (rest args) (args arg))
                      terms
                      coeff))
               (t (with-coeff-term (c term) arg
                    (setf (gethash term terms)
                          (let ((val (gethash term terms)))
                            (if val (cl:+ c val) c)))
                    (iter (rest args) terms coeff))))))
    (multiple-value-bind (coeff hash)
        (iter (args expr) (make-hash-table :test #'equals) 0)
      (let (new-args)
        (maphash (lambda (term coeff)
                   (cond
                     ((null term) (error "bad hash ~A" hash))
                     ((zero? coeff))
                     ((eql coeff 1) (push term new-args))
                     ((typep term '*)
                      (push (apply #'* coeff (args term)) new-args))
                     (t (push (* coeff term) new-args))))
                 hash)
        (string-sort (push coeff new-args))))))
