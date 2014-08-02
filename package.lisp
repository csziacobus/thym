;;;; package.lisp

(defpackage #:thym
  (:use #:cl
        #:anaphora
        #:alexandria
        #:cl-ppcre
        #:cl-lex
        #:equals)
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
