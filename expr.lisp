(in-package #:thym)

(defclass expr ()
  ((%args :accessor args :initarg :args)
   (%maker :accessor maker :initarg :maker)))

(defmacro with-args (args expr &body body)
  `(let ((,args (args ,expr)))
     ,@body))

(defmethod print-object ((expr expr) stream)
  (format stream "~A(~{~a~^, ~})" (type-of expr) (args expr)))

(defmacro defexpr (name direct-superclasses additional-slots
                   lambda-list &body body)
  `(progn
     (defclass ,name ,direct-superclasses
       ,additional-slots
       (:default-initargs :maker #',name))
     (when ',lambda-list
       (defgeneric ,name ,lambda-list)
       (defmethod ,name ,lambda-list
         ,@body))))

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

(defgeneric negative? (expr)
  (:documentation "Is a number negative?")
  (:method ((expr t)) nil))

(defgeneric deriv (expr wrt &optional n)
  (:documentation
   "Takes the derivative of an expression n-times.")
  (:method :around (expr wrt &optional (n 1))
    (if (zerop n)
        expr
        (loop repeat n
              for deriv = (call-next-method) then (deriv deriv wrt)
              finally (return deriv)))))

(defgeneric free-symbols (expr)
  (:documentation "Finds free symbols.")
  (:method ((expr expr))
    (reduce #'union (mapcar #'free-symbols (args expr)))))

(defgeneric coefficient (expr)
  (:documentation "Finds the number term.")
  (:method ((expr expr)) 1))

(defgeneric base (expr)
  (:documentation "Base of exponential.")
  (:method ((expr expr)) expr))

(defgeneric exponent (expr)
  (:documentation "Exponent of exponential.")
  (:method ((expr expr)) 1))

(defgeneric copy (expr &key recursive)
  (:documentation "Copies expression.")
  (:method ((expr expr) &key (recursive t))
    (if recursive
        (func expr (copy (args expr)))
        (func expr (args expr))))
  (:method ((list list) &key (recursive t))
    (if recursive
        (mapcar #'copy (copy-list list))
        (copy-list list)))
  (:method ((expr t) &key recursive)
    (declare (ignore recursive)) expr))

(defgeneric number-free-term (expr)
  (:documentation "Finds the number free term.")
  (:method ((expr expr)) expr))

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
          (if (equals expr old) new expr)
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
