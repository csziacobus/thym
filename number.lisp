(in-package #:thym)

(defun keep-numbers (list)
  (remove-if-not #'number? list))

(defun remove-numbers (list)
  (remove-if #'number? list))

(defmethod deriv ((expr number) wrt &optional (n 1))
  0)

(defgeneric zero? (expr)
  (:documentation "Is an expression zero?")
  (:method ((expr number))
    (zerop expr)))

(defgeneric number? (expr)
  (:documentation "Is it a number?")
  (:method (expr)
    (numberp expr)))

(defmethod free-symbols ((expr number)) nil)
(defmethod number-free-term ((expr number)) nil)
(defmethod coefficient ((expr number)) expr)
(defmethod negative? ((expr number)) (minusp expr))

(defclass numeric-constant ()
  ((value :initform (error "Must supply value.")
          :initarg :value
          :accessor value))
  (:documentation "A mathematical constant with a numerical value."))

(defmethod number? ((expr numeric-constant)) t)
(defmethod zero? ((expr numeric-constant)) (zerop (value expr)))
(defmethod coefficient ((expr numeric-constant)) 1)
(defmethod number-free-term ((expr numeric-constant)) expr)
(defmethod base ((expr numeric-constant)) expr)
(defmethod exponent ((expr numeric-constant)) 1)
(defmethod hash-code ((x numeric-constant)) (value x))

(defmethod equals ((x numeric-constant) (y numeric-constant) &key)
  (= (value x) (value y)))

(defmacro defnumber (name value representation)
  `(progn
     (defparameter ,name
       (make-instance 'numeric-constant :value ,value))
     (defmethod print-object ((object (eql ,name)) stream)
       (princ ',representation stream))))

(defnumber pi cl:pi π)
(defnumber e (cl:exp 1) e)
(defnumber oo 'inf ∞)
(defnumber -oo '-inf -∞)
