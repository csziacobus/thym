;;;; thym.asd

(asdf:defsystem #:thym
  :serial t
  :description "A lispy sym."
  :author "cszi <charleszhang77@gmail.com>"
  :license "GPLv3"
  :depends-on (#:alexandria
               #:equals
               #:iterate)
  :components 
  ((:module core
    :components ((:file "package")
                 (:file "utils")
                 (:file "sort")
                 (:file "expression")
                 (:file "associative")
                 (:file "others")
                 (:file "expt")
                 (:file "multiply")
                 (:file "add")))))

(defpackage #:symcl
  (:use #:common-lisp)
  (:export #:symcl-common-lisp-shadowed-symbols))

#+nil
(asdf:defsystem #:thym
  :serial t
  :description "A lispy sym."
  :author "cszi <charleszhang77@gmail.com>"
  :license "GPLv3"
  :depends-on (#:anaphora
               #:alexandria
               #:cl-ppcre
               #:cl-lex
               #:equals
               #:iterate
               #:cl-who
               #:hunchentoot
               #:parenscript)
  :components 
  ((:file "package")
   (:file "tokenize")
   (:file "infix")
   (:file "utils")
   (:file "expr")
   (:file "number")
   (:file "symbol")
   (:file "function")
   (:file "associative")
   (:file "exponential")
   (:file "^")
   (:file "mul")
   (:file "+")
   (:file "trigonometric")
   (:file "factorials")
   (:file "expr-with-limits")
   (:file "integral")
   (:file "pretty")))
