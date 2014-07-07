(in-package #:thym)

(defexpr trigonometric-function (efun) () ())

(defexpr sin (trigonometric-function) () (arg)
  (if (eql arg 0)
      0
      (make-expr 'sin (list arg))))

(defmethod first-deriv ((fun sin) wrt) (cos (arg fun)))

(defmethod antideriv ((fun sin))
  (lambda (u) (- (cos u))))

(defexpr cos (trigonometric-function) () (arg)
  (if (eql arg 0)
      1
      (make-expr 'cos (list arg))))

(defmethod first-deriv ((fun cos) wrt) (- (sin (arg fun))))

(defmethod antideriv ((fun cos))
  (lambda (u) (sin u)))

(defexpr tan (trigonometric-function) () (arg)
  (make-expr 'tan (list arg)))

(defmethod first-deriv ((fun tan) wrt) (+ 1 (^ fun 2)))

(defexpr cot (trigonometric-function) () (arg)
  (make-expr 'cot (list arg)))

(defmethod first-deriv ((fun cot) wrt) (- -1 (^ fun 2)))

(defexpr asin (fun) () (arg)
  (case arg
    (0 0)
    (1 (/ 'pi 2))
    (-1 (- (/ 'pi 2)))
    (otherwise (make-expr 'asin (list arg)))))

(defmethod first-deriv ((fun asin) wrt)
  (/ (sqrt (- 1 (^ (arg fun) 2)))))

(defexpr acos (fun) () (arg)
  (case arg
    (0 (/ 'pi 2))
    (1 0)
    (-1 'pi)
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
