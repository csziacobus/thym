(in-package #:thym)

(defclass expr ()
	((args :accessor %args :initarg :args)
	 (maker :accessor maker)))

(defmacro defexpr (name direct-superclasses additional-slots
									 lambda-list &body body)
	`(progn
		 (defclass ,name ,direct-superclasses
			 ,additional-slots)
		 (when ',lambda-list
			 (defun ,name ,lambda-list
				 ,@body)
			 (defmethod initialize-instance :after ((instance ,name) &key)
				 (setf (maker instance) ',name)))))

(defun func (instance args)
	(apply (maker instance) args))

(defun make-expr (class args)
	(make-instance class :args args))

(defgeneric atoms (expr &rest types)
	(:documentation
	 "Returns atoms that make up an expression.
    Example: > (atoms (parse 1+x+2*sin(y+i*pi)) 'symbol)
             '(x y)")
	(:method ((expr expr) &rest types)
		(let ((types (or types '(atom))))
			(remove-duplicates
			 (remove-if-not (lambda (expr)
												(some (lambda (type)
																(typep expr type))
															types))
											(preorder-traversal expr))))))

(defgeneric args (expr)
	(:documentation
	 "Returns the args of an expression.")
	(:method ((expr expr))
		(%args expr)))

(defgeneric deriv (expr wrt &optional n)
	(:documentation
	 "Takes the derivative of an expression n-times."))

(defgeneric free-symbols (expr)
	(:documentation
	 "Finds free symbols.")
	(:method ((expr expr))
		(reduce #'union (mapcar #'free-symbols (args expr)))))
	
(defgeneric taylor-term (expr n wrt &rest previous-terms)
	(:documentation
	 "Finds a term in a taylor series.")
	(:method ((expr expr) n wrt &rest previous-terms)
		"Generic, slow, catch-all method."
		(declare (ignore previous-terms))
		(let ((dummy (gensym)))
			(* (subs (subs (deriv (subs expr wrt dummy)
														dummy n)
										 dummy wrt)
							 wrt 0)
				 (^ wrt n)
				 (/ (factorial n))))))

(defgeneric series (expr &key wrt center n dir logx)
	(:documentation "Series expansion of expression.")
	(:method ((expr expr) &key wrt (center 0) (n 6) (dir "+") logx)
		(declare (ignore center dir logx))
		(when (null wrt)
			(let ((symbols (free-symbols expr)))
				(if (or (null symbols) (rest symbols))
						(error "Multivariate or no variable.")
						(setf wrt (first symbols)))))
		(apply #'+ (loop for i from 0 to n
									collect (taylor-term expr i wrt n)))))

(defgeneric equals (x y &rest keys &key recursive &allow-other-keys)
	(:documentation "Generic equality.")
	(:method (x y &key &allow-other-keys)
		(equalp x y))
	(:method ((x cons) (y cons) &key &allow-other-keys)
		(tree-equal x y :test #'equals)))

(defun equals-hash (x)
	(if (typep x 'expr)
			(sxhash (string-sort (args x)))
			(sxhash x)))

(sb-ext:define-hash-table-test equals equals-hash)

(defun preorder-traversal (expr)
	(labels ((%preorder-traversal (expr)
						 (if (typep expr 'expr)
								 (preorder-traversal (args expr))
								 (if (atom expr)
										 expr
										 (cons (preorder-traversal (car expr))
													 (preorder-traversal (cdr expr)))))))
		(flatten (%preorder-traversal expr))))

(defun subs (expr old new)
	"Traverses tree and substitutes symbols. Non-destructive but slow."
	(if (typep expr 'expr)
			(func expr (subs (args expr) old new))
			(if (atom expr)
					(if (eql expr old) new expr)
					(cons (subs (car expr) old new)
								(subs (cdr expr) old new)))))

(defmethod zero? ((expr expr)) nil)

(defun - (arg &rest other-args)
	(if other-args
			(+ arg (* (apply #'+ other-args) -1))
			(* arg -1)))

(defun / (arg &rest other-args)
	(if other-args
			(* arg (^ (apply #'* other-args) -1))
			(^ arg -1)))

(defun sqrt (arg) (^ arg 1/2))
