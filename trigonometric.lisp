(in-package #:thym)

(defparameter +unit-circle+
  `(0  (sin ,0                  cos ,1
        tan ,0                  cot ,oo)   
    2  (sin ,1/2                cos ,(/ (sqrt 2) 2))
    3  (sin ,(/ (sqrt 2) 2)     cos ,(/ (sqrt 2) 2))
    4  (sin ,(/ (sqrt 3) 2)     cos ,1/2)
    6  (sin ,1                  cos ,0)
    8  (sin ,(/ (sqrt 3) 2)     cos ,-1/2)
    9  (sin ,(/ (sqrt 2) 2)     cos ,(/ (sqrt 2) -2))
    10 (sin ,1/2                cos ,(- (/ (sqrt 3) 2)))
    12 (sin ,0                  cos ,-1)
    14 (sin ,-1/2               cos ,(- (/ (sqrt 3) 2)))
    15 (sin ,(- (/ (sqrt 2) 2)) cos ,(- (/ (sqrt 2) 2)))
    16 (sin ,(- (/ (sqrt 3) 2)) cos ,-1/2)
    18 (sin ,-1                 cos ,0)
    20 (sin ,(- (/ (sqrt 3) 2)) cos ,1/2)
    21 (sin ,(- (/ (sqrt 2) 2)) cos ,(/ (sqrt 2) 2))
    22 (sin ,-1/2               cos ,(/ (sqrt 3) 2))))

(defexpr trigonometric-function (efun) () (class angle)
  (make-expr class (list angle)))

(defmacro define-trigonometric-function (name)
  `(defexpr ,name (trigonometric-function) () (angle)
     (if (or (typep angle '*) (zero? angle))
         (let ((coefficient (coefficient-wrt angle pi)))
           (or (getf (getf +unit-circle+
                           (mod (* coefficient 12) 24))
                     ',name)
               (trigonometric-function ',name angle)))
         (trigonometric-function ',name angle))))

(define-trigonometric-function sin)
(defmethod first-deriv ((fun sin) wrt) (cos (arg fun)))
(defmethod antideriv ((fun sin)) (lambda (u) (- (cos u))))

(define-trigonometric-function cos)
(defmethod first-deriv ((fun cos) wrt) (- (sin (arg fun))))
(defmethod antideriv ((fun cos)) (lambda (u) (sin u)))

(define-trigonometric-function tan)
(defmethod first-deriv ((fun tan) wrt) (+ 1 (^ fun 2)))

(define-trigonometric-function cot)
(defmethod first-deriv ((fun cot) wrt) (- -1 (^ fun 2)))

(defexpr asin (fun) () (arg)
  (case arg
    (0 0)
    (1 (/ pi 2))
    (-1 (- (/ pi 2)))
    (otherwise (make-expr 'asin (list arg)))))

(defmethod first-deriv ((fun asin) wrt)
  (/ (sqrt (- 1 (^ (arg fun) 2)))))

(defexpr acos (fun) () (arg)
  (case arg
    (0 (/ pi 2))
    (1 0)
    (-1 pi)
    (otherwise (make-expr 'acos (list arg)))))

(defmethod first-deriv ((fun acos) wrt)
  (- (sqrt (- 1 (^ (arg fun) 2)))))

(defexpr atan (fun) () (arg)
  (case arg
    (0 0)
    (1 (/ 'pi 4))
    (-1 (- (/ 'pi 4)))
    (otherwise (make-expr 'atan (list arg)))))

(defmethod first-deriv ((fun atan) wrt)
  (/ (+ 1 (^ (arg fun) 2))))

(defexpr acot (fun) () (arg)
  (make-expr 'acot (list arg)))

(defmethod first-deriv ((fun acot) wrt)
  (/ -1 (+ 1 (^ (arg fun) 2))))
