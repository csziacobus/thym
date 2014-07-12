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
      ((notany (lambda (sym)
                 (member sym vars))
               (free-symbols function))
       (apply #'* function vars))
      ((symbolp function)
       (/ (^ function 2) 2))
      ((deriv-divide function (first vars))
       (multiple-value-bind (op u du c)
           (deriv-divide function (first vars))
         (declare (ignore du))
         (* c (funcall (antideriv op) u))))
      ((typecase function
         (+
          (apply #'+ (mapcar (lambda (arg)
                               (apply #'integrate arg limits))
                             (args function))))
         (*
          (when (nth-value 1 (coefficient function))
            (with-coeff-term (coeff term) function
              (* coeff (apply #'integrate term limits)))))
         (^
          (apply #'integrate (expand function) limits))))
      ((typep function '*)
       (integrate-by-parts function limits))
      (t expr))))

(defun integrate (function &rest limits)
  (if (and (singlep limits) (symbolp (first limits)))
      (try (apply #'integral function (list limits)))
      (try (apply #'integral function limits))))

(defun integrate-by-parts (function limits)
  (let (dv u v du (args (args function)))
    (dolist (arg args)
      (when (typep arg '(or symbol efun))
        (setf dv arg
              u (apply #'* (remove dv args))
              v (apply #'integrate dv limits)
              du (deriv u (first (first limits))))))
    (- (* u v) (apply #'integrate (* v du) limits)))) 

(defun map-all (function list)
  (let ((length (length list)) new)
    (if (<= length 2)
        list
        (dotimes (i length new)
          (map-combinations (lambda (arg)
                              (push (apply function arg) new))
                            (copy list)
                            :length i)))))

(defun deriv-divide (expr wrt)
  (check-type wrt symbol)
  (with-coeff-term (coeff term) expr
    (let ((terms (if (symbolp term)
                     term
                     (map-all '* (args term))))
          op u du c)
      (if (typep term '*) 
          (dolist (mop (args term))
            (dolist (mdu terms)
              (unless (or (eq mop mdu) op)
                (when (typep mop 'efun)
                  (let ((adu (deriv (arg mop) wrt)))
                    (when (equals mdu (number-free-term adu))
                      (setf op mop 
                            u (arg mop)
                            du adu
                            c (coefficient adu)))))
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
            (when (typep mop 'efun)
              (let ((adu (deriv (arg mop) wrt)))
                (when (number? adu)
                  (setf op mop
                        u (arg mop)
                        du adu
                        c (coefficient adu)))))
            (when (and (not op) (typep mop '^))
              (let ((adu (deriv (base mop) wrt)))
                (when (number? adu)
                  (setf op mop
                        u (base mop)
                        du adu
                        c (coefficient adu)))))
            (unless op
              (let ((adu (deriv mop wrt)))
                (when (number? adu)
                  (setf op mop
                        u mop
                        du adu
                        c (coefficient adu)))))))
      (when (and op (equals op u))
        (setf op 'ident))
      (values op u du (and c (/ coeff c))))))

(defmethod antideriv ((expr (eql 'ident)))
  (lambda (u) (/ (^ u 2) 2)))
