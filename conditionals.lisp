(in-package #:thym)

(defclass interval ()
  ((low :initarg :low :accessor low)
   (mid :initarg :mid :accessor mid)
   (high :initarg :high :accessor high))
  (:default-initargs
   :low -oo
   :high oo))

(defun make-interval (mid &optional low high)
  (make-instance 'interval :mid mid :low low :high high))

(defmethod print-object ((expr interval) stream)
  (format stream "(~a < ~a < ~a)"
          (low expr) (mid expr) (high expr)))

(defmethod 2arg+ ((x interval) (y interval))
  (make-interval (+ (mid x) (mid y))
                 (+ (low x) (low y))
                 (+ (high x) (high y))))

(defmethod 2arg+ ((x (eql oo)) y)  oo)
(defmethod 2arg+ ((x (eql -oo)) y) -oo)
(defmethod 2arg+ ((x (eql -oo)) (y (eql oo))) 0)
(defmethod 2arg+ ((x (eql oo)) (y (eql -oo))) 0)
