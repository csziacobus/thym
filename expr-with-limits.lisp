(in-package #:thym)

(defexpr expr-with-limits (expr) () ())

(defmacro with-limits-expr ((function limits &optional vars) instance &body body)
  (with-gensyms (g)
    `(let ((,function (fun ,instance))
           (,limits (limits ,instance))
           ,(if vars `(,vars (vars ,instance)) g))
       ,(if vars nil `(declare (ignore ,g)))
       ,@body)))

(defmethod fun ((expr expr-with-limits))
  (first (args expr)))

(defmethod limits ((expr expr-with-limits))
  (rest (args expr)))

(defmethod vars ((expr expr-with-limits))
  (mapcar #'first (limits expr)))

;;; ugly imperative
(defmethod free-symbols ((expr expr-with-limits))
  (with-limits-expr (function limits) expr
    (let ((free (free-symbols function)))
      (dolist (limit limits (flatten free))
        (cond ((singlep limit)
               (pushnew (first limit) limit))
              (t (when (member (first limit) free)
                   (setf free (delete (first limit) free)))
                 (dolist (i (rest limit))
                   (pushnew (free-symbols i) free))))))))

(defexpr add-with-limits (expr-with-limits) () ())
