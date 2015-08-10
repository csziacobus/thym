(in-package #:symcl/core)

(defvar *associative-identities* (make-hash-table :test #'eq))

(defclass associative-operator ()
  ((%commutativep :accessor commutativep :initarg :commutativep))
  (:documentation
   "Associative, can seperate noncommutative and commutative."))

(defun associative-identity (operator)
  (gethash operator *associative-identities*))

(defmacro define-associative-operator
    (operator &key (combine-like-terms-by #'identity)
                   additional-slots
                   additional-superclasses
                   identity)
  `(progn
     (defclass ,operator (expression associative-operator ,@additional-superclasses)
       ,additional-slots)
     (setf (gethash ',operator *associative-identities*) ,identity)
     (defun ,operator (&rest args)
       (macrolet ((filter ((filtered-args args) &body body)
                    `(let ((args (remove ,,identity ,args)))
                       (cond ((null args) ,,identity)
                             ((null (rest args)) (first args))
                             (t (let ((,filtered-args (remove ,,identity ,args)))
                                  ,@body))))))
         (filter (filtered-args args)
           (filter (filtered-args (,combine-like-terms-by filtered-args))
             (make-instance ',operator :args filtered-args)))))))

(defun update-or-add-key (alist key value)
  (check-type alist (or null (cons cons)) "an alist")
  (let ((cons (assoc key alist :test #'expression=)))
    (if cons
        (progn (setf (cdr cons) (+ (cdr cons) value))
               alist)
        (acons key value alist))))

(defmacro with-like-terms ((constant terms args operator
                            &key constant-fold partition identity)
                           &body body)
  "Binds constant and terms with args constant folded.
Partition delineates a function that returns both the count of
the term as the first value and the term itself as the second ."
  (with-gensyms (rec)
    `(labels ((,rec (args terms constant)
                (let ((arg (first args)))
                  (typecase arg
                    (null (values constant terms))
                    (number (,rec (rest args)
                                  terms
                                  (,constant-fold constant arg)))
                    (,operator (,rec (append (args arg) (rest args))
                                     terms
                                     constant))
                    (otherwise 
                     (multiple-value-bind (c term)
                         (,partition arg)
                       (,rec (rest args)
                             (update-or-add-key terms term c)
                             constant)))))))
       (multiple-value-bind (,constant ,terms)
           (,rec ,args () ,identity)
         ,@body))))
