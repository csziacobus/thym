(in-package #:thym)

(defun print-latex (expr &optional (stream *standard-output*))
  (princ (write-latex expr) stream))

(defgeneric write-latex (expr)
  (:documentation "Latex writer.")
  (:method ((number number))
    (if (and (rationalp number) (/= (denominator 1) 1))
        (format nil "\\frac{~d}{~d}"
                (numerator number)
                (denominator number))
        (write-to-string number)))
  (:method ((expr symbol))
    (string-downcase (symbol-name expr)))
  (:method ((expr +))
    (with-output-to-string (stream)
      (with-args args expr
        (print-latex (first args) stream)
        (dolist (arg (rest args))
          (cond ((negative? arg)
                 (format stream " - ~A" (write-latex (- arg))))
                (t
                 (format stream " + ~A" (write-latex arg))))))))
  (:method ((expr *))
    (let ((tex "") (args (args expr)))
      (when (negative? (coefficient expr))
        (setf tex "- " expr (- expr)))
      (labels ((convert (expr)
                 (if (not (typep expr '*))
                     (return-from convert (write-latex expr))
                     (let ((tex ""))
                       (dotimes (i (length args) tex)
                         (let* ((term (nth i args))
                                (term-tex (write-latex term)))
                           (when (*bracketp term
                                            (null (nthcdr (1+ i)
                                                          args)))
                             (setf term-tex
                                   (format nil "\\left(~a\\right)" 
                                           term-tex)))
                           (setf tex (format nil "~a~a" tex
                                             term-tex))))))))
        (format nil "~a~a" tex (convert expr)))))
  (:method ((expr efun))
    (let ((function-name (funcall (compose #'string-downcase
                                           #'string
                                           #'type-of)
                                  expr)))
      (format nil (if (bracketp (arg expr))
                      "\\~a\\left(~a\\right)"
                      "\\~a ~a")
              function-name (write-latex (arg expr)))))
  (:method ((expr ^))
    (let (tex)
      (with-base-exponent (base exponent) expr
        (if (and (rationalp exponent)
                 (= (abs (numerator exponent)) 1)
                 (/= (denominator exponent) 1))
            (let* ((q (denominator exponent)))
              (setf tex (if (= q 2)
                            (format nil "\\sqrt{~a}"
                                    (write-latex base))
                            (format nil "\\sqrt[~d]{~a}"
                                    q
                                    (write-latex base))))
              (if (negative? exponent)
                  (return-from write-latex
                    (format nil "\\frac{1}{~a}" tex))
                  (return-from write-latex tex)))
            (progn
              (setf tex
                    (format nil
                            (cond 
                              ((eql -1 exponent) "~a")
                              ((bracketp base)
                               "\\left(~a\\right)^{~a}")
                              (t "~a^{~a}"))
                            (write-latex base)
                            (write-latex  (if (negative? exponent)
                                              (- exponent)
                                              exponent))))
              (when (negative? exponent)
                (setf tex (format nil "\\frac{1}{~a}" tex)))
              tex)))))
  (:method ((expr exp))
    (format nil "e^{~a}" (write-latex (arg expr))))
  (:method ((expr (eql pi))) "\\pi")
  (:method ((expr (eql e))) "e")
  (:method ((expr (eql oo))) "\\infty")
  (:method ((expr (eql -oo))) "-\\infty")
  (:method ((expr integral))
    (let ((tex ""))
      (with-limits-expr (fun limits vars) expr
        (cond
          ((and (< (length limits) 4) (every (lambda (lim)
                                               (null (rest lim)))
                                             limits))
           (setf tex (format nil "\\i~v@{~A~:*~}nt"
                             (1- (length limits)))))
          (t (dolist (lim (reverse limits))
               (setf tex (format nil "~a\\int" tex))
               (setf tex
                     (case (length lim)
                       (3 (format nil "~a_{~a}^{~a}" tex
                                      (write-latex (second lim))
                                      (write-latex (third lim))))
                       (2 (format nil "~a^{~a}" tex
                                  (write-latex (second lim)))))))))
        (format nil "~a ~a~{~a~}" tex (write-latex fun)
                (mapcar (lambda (sym)
                          (format nil "\\, d~a" (write-latex sym)))
                        vars))))))

(defun bracketp (expr)
  (not (or (and (integerp expr) (negative? expr))
           (and (not (typep expr 'expr)) (not (eql expr -1))))))

(defun *bracketp (expr &optional last)
  (or (typep expr '+) (and (typep expr 'expr) 
                           (not last)
                           (some (lambda (type)
                                   (some (lambda (arg)
                                           (typep arg type))
                                         (args expr)))
                                 '(integral #+nil sum)))))
