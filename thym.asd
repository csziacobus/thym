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
  :components ((:file "package")
	       (:file "utils")
	       (:file "infix")
	       (:file "tokenize")
               (:file "thym")))

