(in-package #:thym)

(defexpr assoc-expr (expr)
    ((%commutativep :initarg :commutativep))
    (class args)
  (let ((identity (associative-identity class)))
    (macrolet ((filter-trivial (args &body body)
                 `(let ((,args (remove identity ,args)))
                    (cond ((null ,args) identity)
                          ((singlep ,args) (first ,args))
                          (t ,@body)))))
      (filter-trivial args
        (let ((new-args (collect-like-terms (make-expr class args))))
          (filter-trivial new-args
            (make-expr class new-args)))))))

(defgeneric associative-identity (assoc-expr)
  (:documentation "The identity of an associative expression."))

(defmacro define-associative-expr (name identity)
  `(progn
     (defexpr ,name (assoc-expr) () (&rest args)
       (assoc-expr ',name args))
     (defmethod associative-identity ((expr (eql ',name)))
       ,identity)))

(defmethod print-object ((expr assoc-expr) stream)
  (let ((list (intersperse-with-precedence expr)))
    (format stream "~A~{ ~A~}" (first list) (rest list))))

(defgeneric collect-like-terms (expr)
  (:documentation "Collect like-terms, returning new arguments."))

(defgeneric level (expr)
  (:documentation "Generic reducing for associative operators."))

(defmethod equals ((x assoc-expr) (y assoc-expr) &key &allow-other-keys)
  (equals (string-sort (args x)) (string-sort (args y))))
