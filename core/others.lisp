(in-package #:symcl/core)

(macrolet ((def (fun inverse higher-order-fun)
             `(defun ,fun (arg &rest more-args)
                (if more-args
                    (apply #',inverse arg
                           (mapcar (lambda (x)
                                     (,higher-order-fun x -1))
                                   more-args))
                    (,higher-order-fun arg -1)))))
  (def - + *)
  (def / * ^))
