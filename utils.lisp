;;;; utils.lisp

(in-package #:thym)

(defun singlep (list)
  (and (first list) (null (rest list))))

(defun keep-numbers (list)
  (remove-if-not #'numberp list))

(defun remove-numbers (list)
  (remove-if #'numberp list))

(defun strings (&rest args)
  (apply #'concatenate 'string args))
