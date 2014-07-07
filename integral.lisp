(in-package #:thym)

(defexpr integral (add-with-limits) () (function &rest limits)
  "Unevaluated integral."
  (make-expr 'integral (list* function limits)))

(defmethod print-object ((expr integral) stream)
  (format stream "ʃ ~A d~{~A~}" (fun expr) (limits expr)))

(defun equal-bounds-p (limits)
  (some (lambda (limit)
          (declare (cons limit))
          (and (length= 3 limit)
               (eql (second limit) (third limit))))
        limits))

(defmethod free-symbols ((expr integral))
  "Gets the symbols that weren't dummies of integration,
   Example: > (free-symbols (integral 'x '(x y 1)))
              (y)
            > (free-symbols (integral 'x '(x y z)))
              (y z)"
  (if (equal-bounds-p (limits expr))
      nil
      (call-next-method)))

; fix, ugly imperative, python inspired code
(defmethod zero? ((expr integral))
  "Trivial tests for zero-ness."
  (if (or (zero? (fun expr)) ; fun is zero or upper = lower
          (equal-bounds-p (limits expr)))
      t
      (let ((free (free-symbols (fun expr))))
        (if (and (null (free-symbols expr))
                 (numberp (fun expr)))
            nil
            (dolist (limit (limits expr))
              (cond ((singlep limit)
                     (pushnew (first limit) free))
                    ((and (length= limit 2)
                          (zero? (second limit))
                          (not (member (first limit) free)))
                     (return t))
                                        ; take dummy symbol out
                    (t (remove (first limit) free)
                                        ; add in new symbol
                       (dolist (i (rest limit))
                         (pushnew (free-symbols i) free)))))))))

;; ditto
(defmethod number? ((expr integral))
  "Test if integral will evaluate to some number."
  (with-limits-expr (integrand limits) expr
    (let ((free (free-symbols integrand)))
      (dolist (limit limits (null free))
        (declare (list limit))
        (cond ((singlep limit)
               (pushnew (first limit) free))
              ((length= 3 limit)
               (eql (second limit) (third limit))
               (return t))
              ((member (first limit) free)
               (remove (first limit) free))
              (t
               (when (length= 3 limit)
                 (pushnew (free-symbols
                           (set-difference (second limit)
                                           (first limit)))
                          free))
               (dolist (i (rest limit))
                 (pushnew (free-symbols i) free))))))))

(defmethod try ((expr integral))
  (with-limits-expr (function limits vars) expr
    (cond
      ((zero? function) 0)
      ((symbolp function)
       (if (member function vars)
           (/ (^ function 2) 2)
           (* function (first vars))))
      ((and (typep function '^)
            (member (base function) vars))
       (funcall (antideriv function) (base function)))
      ((and (typep function 'efun)
            (member (arg function) vars))
       (funcall (antideriv function) (arg function)))
      ((typecase function
         (+
          (apply #'+ (mapcar (lambda (arg)
                               (apply #'integrate arg limits))
                             (args function))))
         (*
          (cond
            ((/= 1 (coefficient function))
             (with-coeff-term (coeff term) function
               (* coeff (apply #'integrate term limits))))
            ((deriv-divide function (first vars))
             (multiple-value-bind (op u du c)
                 (deriv-divide function (first vars))
               (declare (ignore du))
               (* c (funcall (antideriv op) u))))
            (t expr)))
         (^
          (apply #'integrate (expand function) limits))
         (otherwise expr)))
      (t expr))))

(defun integrate (function &rest limits)
  (try (apply #'integral function limits)))

(defun map-all (function list)
  (let ((length (length list)) new)
    (dotimes (i length (nreverse new))
      (map-combinations (lambda (arg)
                          (push (apply function arg) new))
                        list
                        :length i))))

(defun deriv-divide (expr wrt)
  (check-type expr *)
  (with-coeff-term (coeff term) expr
    (let ((terms (map-all '* (args term)))
          op u du c)
      (if (typep term '*) 
          (dolist (mop (args term))
            (dolist (mdu terms)
              (unless (or (eq mop mdu) op)
                (when (typep mop 'efun)
                  (let ((adu (deriv (arg mop) wrt)))
                    (when (equals mdu (number-free-term adu))
                      (setf op mop 
                            u mop
                            du adu
                            c (print (coefficient adu))))))
                (when (and (not op) (typep mop '^))
                  (let ((adu (deriv (base mop) wrt)))
                    (when (equals mdu (number-free-term adu))
                      (setf op mop
                            u (base mop)
                            du adu
                            c (coefficient adu)))))
                (unless op
                  (let ((adu (deriv mop wrt)))
                    (when (equals mdu (number-free-term adu))
                      (setf op mop
                            u mop
                            du adu
                            c (coefficient adu))))))))      
          (let ((mop term))
            (cond
              ((typep mop 'efun)
               (let ((adu (deriv (arg mop) wrt)))
                 (setf op mop
                       u (arg mop)
                       du adu
                       c (coefficient adu)))))))
      (when (and op (equals op u))
        (setf op 'ident))
      (values op u du (/ coeff c)))))

(defmethod antideriv ((expr (eql 'ident)))
  (lambda (u) (/ (^ u 2) 2)))
