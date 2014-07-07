(in-package #:thym)

(defun keep-numbers (list)
  (remove-if-not #'numberp list))

(defun remove-numbers (list)
  (remove-if #'numberp list))

(defmethod deriv ((expr number) wrt &optional (n 1))
  0)

(defgeneric zero? (expr)
  (:documentation "Is an expression zero?")
  (:method ((expr number))
    (zerop expr)))

(defgeneric number? (expr)
  (:documentation "Is the integral a number?")
  (:method (expr)
    (numberp expr)))

(defmethod free-symbols ((expr number)) nil)
(defmethod number-free-term ((expr number)) nil)
(defmethod coefficient ((expr number)) expr)
