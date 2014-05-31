;;;; delta.lisp

(in-package #:thym)

(defun delta-op (op) (symbolicate "delta" op))

(defun sym-infix (expr)
  "For use internally."
  (funcall (compose #'sym
		    #'infix->prefix
		    #'parenthesize)
	   expr))                            

(defmacro defdelta (op parameters &body body)
  (let ((name (delta-op op)))
    `(defun ,name ,parameters
       (when (notany (curry #'function-of ,(first parameters))
		     (list ,@(rest parameters)))
	 (return-from ,name 0))
       ,@body)))

(defdelta sin (wrt x)
  (sym-infix `((delta ,x ,wrt) * (cos ,x))))

(defdelta cos (wrt x)
  (sym-infix `((delta ,x ,wrt) * (- sin ,x))))

(defdelta tan (wrt x)
  (sym-infix `((delta ,x ,wrt) * (1 / (cos ,x) ^ 2))))

(defdelta sinh (wrt x)
  (sym-infix `((delta ,x ,wrt) * (cosh ,x))))

(defdelta cosh (wrt x)
  (sym-infix `((delta ,x ,wrt) * (sinh ,x))))

(defdelta tanh (wrt x)
  (sym-infix `((delta ,x ,wrt) * (1 / (cosh ,x) ^ 2))))

(defdelta asin (wrt x)
  (sym-infix `((delta ,x ,wrt) * (1 / (1 - x ^ 2) ^ 1/2))))

(defdelta acos (wrt x)
  (sym-infix `((delta ,x ,wrt) * (- 1 / (1 - x ^ 2) ^ 1/2))))

(defdelta atan (wrt x)
  (sym-infix `((delta ,x ,wrt) * (1 / (cosh x) ^ 2))))

(defdelta asinh (wrt x)
  (sym-infix `((delta ,x ,wrt) * (1 / (1 + x ^ 2) ^ 1/2))))

(defdelta acosh (wrt x)
  (sym-infix `((delta ,x ,wrt) * (1 / ((x - 1) * (x + 1)) ^ 1/2))))

(defdelta atanh (wrt x)
  (sym-infix `((delta ,x ,wrt) * (1 / (1 - x ^ 2)))))

(defdelta log (wrt x)
  (sym-infix `((delta ,x ,wrt) * (1 / ,x))))

(defdelta + (wrt x y)
  (sym-infix `((delta ,x ,wrt) + (delta ,y ,wrt))))

(defdelta - (wrt x y)
  (sym-infix `((delta ,x ,wrt) - (delta ,y ,wrt))))

(defdelta * (wrt x y)
  (sym-infix `(,y * (delta ,x ,wrt) + ,x * (delta ,y ,wrt))))

(defdelta / (wrt x y)
  (sym-infix `((delta ,x ,wrt) * ,y - (delta ,y ,wrt) * ,x /
	       ,y ^ 2)))

(defdelta ^ (wrt x y)
  (sym-infix `((delta ,x ,wrt) * ,y * ,x ^ (,y - 1)	; Power rule
	       +
	       ((delta ,y ,wrt) * (log ,x) * ,x ^ ,y)))) ; Exponent rule

(defun delta (expr wrt)
  (if (atom expr)
      (if (equal expr wrt) 1 0)
      (if (fboundp (delta-op (op expr)))
	  (apply (delta-op (op expr))
		 wrt
		 (args expr))
	  `(delta ,(op expr) ,@(args expr) ,wrt))))

(defsym delta (expr wrt)
  (sym (delta expr wrt)))
