(in-package #:thym)

(defexpr * (assoc-expr) ((identity :initform 1
                                   :allocation :class))
    (&rest args)
  (assoc-expr '* args))


(defmethod deriv ((expr *) wrt &optional (n 1))
  (let ((args (args expr)) factors)
    (dotimes (i (length args))
      (let ((term (deriv (nth i args) wrt n)))
        (unless (eql term 0)
          (push (apply #'*
                       (append (nthcdr (1+ i) args)
                               (list term)
                               (subseq args 0 i)))
                factors))))
    (apply #'+ (nreverse factors))))

(defmethod level ((expr *))
  (let* ((args (mapcan (lambda (arg)
                         (if (typep arg '*)
                             (args arg)
                             (list arg)))
                       (args expr)))
         (powers (keep '^ args))
         (singles (remove-duplicates (keep-symbols args)
                                     :test #'eq))
         (bases (remove-duplicates (mapcar #'base powers)
                                   :test #'equals)))
    (string-sort
     (append
      (list (reduce 'cl:* (keep-numbers args)))
      (remove-numbers (remove-symbols (flunk '^ args)))
      (mapcar (lambda (var)
                (^ var
                   (apply #'+
                          (count var args)
                          (mapcar #'exponent
                                  (remove-if-not
                                   (lambda (x)
                                     (equals (base x) var))
                                   powers)))))
              (union bases singles :test #'eq))))))

(defmethod as-coeff-mul ((expr *))
  (let ((args (args expr)))
    (if (numberp (first args))
        (values (first args)
                (apply #'* (rest args)))
        (values 1 expr))))