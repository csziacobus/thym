;;;; thym.asd

(asdf:defsystem #:thym
  :serial t
  :description "A lispy sym."
  :author "cszi <charleszhang77@gmail.com>"
  :license "GPLv3"
  :depends-on (#:anaphora
               #:alexandria
               #:cl-ppcre
               #:cl-lex)
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
   (:file "*")
   (:file "exponential")
   (:file "^")
   (:file "trigonometric")
   (:file "factorials")))
