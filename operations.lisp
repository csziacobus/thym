(in-package #:thym)

(defexpr assoc-expr (expr)
		(identity
		 (commutativep :initarg :commutativep))
		(class args)
	(let ((identity (class-value class 'identity)))
		(macrolet ((filter-trivial (args &body body)
								 `(let ((,args (remove identity ,args)))
										(cond ((null args) identity)
													((singlep args) (first args))
													(t ,@body)))))
			(filter-trivial args
			  (let ((new-args (level (make-expr class args))))
					(filter-trivial new-args
					  (make-expr class new-args)))))))

(defmethod print-object ((expr assoc-expr) stream)
	(let ((list (intersperse-with-precedence expr)))
		(format stream "~A~{ ~A~}" (first list) (rest list))))

(defgeneric level (expr)
	(:documentation
	 "Generic reducing for associative operators."))

(defmethod equals ((x assoc-expr) y &key &allow-other-keys) nil)
(defmethod equals (x (y assoc-expr) &key &allow-other-keys) nil)

(defmethod equals ((x assoc-expr) (y assoc-expr) &key &allow-other-keys)
	(equals (string-sort (args x)) (string-sort (args y))))
