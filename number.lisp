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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass numeric-constant ()
    ((value :initform (error "Must supply value.")
            :initarg :value
            :accessor value))
    (:documentation "A mathematical constant with a numerical value.")))

(defmethod number? ((expr numeric-constant)) t)
(defmethod zero? ((expr numeric-constant)) (zerop (value expr)))

(defconstant pi
  (make-instance 'numeric-constant :value cl:pi))

(defmethod print-object ((object (eql pi)) stream)
  (princ "π" stream))

(defconstant e
  (make-instance 'numeric-constant :value (cl:exp 1)))

(defmethod print-object ((object (eql e)) stream)
  (princ "e" stream))
