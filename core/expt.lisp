(in-package #:symcl/core)

(defclass ^ (expression)
  ((%commutativep :accessor commutativep :initarg :commutativep)))

(macrolet ((def (fun accessor)
             `(defmethod ,fun ((expression ^))
                (,accessor (args expression)))))
  (def base first)
  (def exponent second))

(defmacro with-base-exponent ((base exponent) expression &body body)
  `(let ((,base (base ,expression))
         (,exponent (exponent ,expression)))
     ,@body))

(defun ^ (base exponent)
  (cond ((and (numberp base) (numberp exponent))
         (expt base exponent))
        ((case exponent
           (0 1)
           (1 base)))
        ((case base
           (1 1)
           (exp1 (exp exponent))))
        (t (when (and (integerp exponent)
                      (minusp (*-coefficient base)))
             (setf base (- base))
             (when (oddp exponent)
               (return-from ^ (- (^ base exponent)))))
           (make-instance '^ :args (list base exponent)))))
