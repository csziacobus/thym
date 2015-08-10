;;;; package.lisp

(defpackage thym
  (:use #:cl
        #:anaphora
        #:alexandria
        #:cl-ppcre
        #:cl-lex
        #:equals
        #:iter)
  (:shadow arg
           exp
           log
           sin
           cos
           factorial
           +
           *
           -
           /
           tan
           sqrt
           asin
           acos
           atan
           pi))

(defpackage thym-web
  (:use #:cl
        #:hunchentoot
        #:cl-who
        #:parenscript))
