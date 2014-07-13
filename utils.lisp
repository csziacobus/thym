;;;; utils.lisp

(in-package #:thym)

(defun singlep (list)
  (and (first list) (null (rest list))))

(defun find-number (list)
  (find-if #'numberp list))

(defun strings (&rest args)
  (format nil "~{~A~}" args))

(defun keep (class sequence)
  (remove-if-not (of-type class) sequence))

(defun flunk (class sequence)
  (remove-if (of-type class) sequence))

(defun class-value (class slot-name)
  (slot-value (make-instance class) slot-name))

;; * relies on numbers sorting first 
(defun string-sort (sequence)
  (sort (copy-list sequence) (lambda (elt1 elt2)
                               (string< (strings elt1)
                                        (strings elt2)))))  
  
