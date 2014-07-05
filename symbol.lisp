(in-package #:thym)

(defun keep-symbols (list)
  (remove-if-not #'symbolp list))

(defun remove-symbols (list)
  (remove-if #'symbolp list))

(defmethod deriv ((expr symbol) wrt &optional (n 1))
  (if (eq expr wrt)
      1
      0))

(defmethod free-symbols ((expr symbol)) (list expr))
(defmethod zero? ((expr symbol)) nil)
