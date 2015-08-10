(in-package #:symcl/core)

(defmacro doalist ((key val alist &optional result) &body body)
  (with-gensyms (pair)
    `(dolist (,pair ,alist ,result)
       (let ((,key (car ,pair)) (,val (cdr ,pair)))
         ,@body))))
