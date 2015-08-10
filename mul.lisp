(in-package #:thym)

(define-associative-expr * 1)

(defmethod deriv ((expr *) wrt &optional (n 1))
  (let ((args (args expr)))
    (apply #'+
           (iter (for i from 0 to (length args))
             (let ((term (deriv (nth i args) wrt)))
               (unless (eql term 0)
                 (collect (apply #'*
                                 term
                                 (append (nthcdr (1+ i) args)
                                         (subseq args 0 i))))))))))

(defmethod collect-like-terms ((expr *))
  (when (member 0 (args expr))
    (return-from collect-like-terms '(0)))
  (labels ((rec (args terms coeff &aux (arg (first args)))
             (cond
               ((null args) (values coeff terms))
               ((numberp arg)
                (rec (rest args) terms (cl:* coeff arg)))
               ((typep arg '*)
                (rec (append (rest args) (args arg))
                     terms
                     coeff))
               (t (with-base-exponent (base exponent) arg
                    (rec (rest args)
                         (let ((val (assoc base terms :test #'equals)))
                           (if val
                               (prog1 terms
                                 (setf (cdr val)
                                       (cl:+ exponent (cdr val))))
                               (acons base exponent terms)))
                         coeff))))))
    (multiple-value-bind (coeff alist) (rec (args expr) () 1)
      (string-sort
       (cons coeff (iter (for (base . exponent) in alist)
                     (cond ((zero? base) (collect base))
                           ((eql exponent 1) (collect base))
                           ((zero? exponent))
                           ((eql base 1))
                           (t (collect (^ base exponent))))))))))

(defmethod coefficient ((expr *))
  (let ((numbers (keep-numbers (args expr))))
    (values (apply #'* numbers)
            (not (eq (args expr) numbers)))))

(defmethod number-free-term ((expr *))
  (let ((number-free (remove-numbers (args expr))))
    (values (apply #'* number-free)
            (not (eq (args expr) number-free)))))

(defun coefficient-wrt (expr wrt)
  (if (zero? expr)
      0
      (apply #'* (remove wrt (args expr) :test #'equals))))

(defmacro with-coeff-term ((coefficient number-free-term) expr &body body)
  `(let ((,coefficient (coefficient ,expr))
         (,number-free-term (number-free-term ,expr)))
     ,@body))

(defmethod negative? ((expr expr))
  (member-if (lambda (arg)
               (and (numberp arg) (minusp arg)))
             (args expr)))
