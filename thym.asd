;;;; thym.asd

(asdf:defsystem #:thym
  :serial t
  :description "A lispy sym."
  :author "cszi <charleszhang77@gmail.com>"
  :license "GPLv3"
  :depends-on (#:anaphora
               #:alexandria
               #:cl-ppcre
               #:cl-lex
               #:equals)
  :components
  ((:file "package")
   (:file "tokenize")
   (:file "infix")
   (:file "utils")
   (:file "expr")
   (:file "number")
   (:file "symbol")
   (:file "function")
   (:file "operations")
   (:file "+")
   (:file "mul")
   (:file "exponential")
   (:file "^")
   (:file "trigonometric")
   (:file "factorials")
   (:file "expr-with-limits")
   (:file "integral")))
