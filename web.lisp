(in-package #:thym-web)

(defvar *http* (start (make-instance 'easy-acceptor :port 8080)))
(setf (html-mode) :html5)

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
       (:head
         (:meta :charset "utf-8")
         (:title ,title)
         (:link :href "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
                :rel "stylesheet")
         (:script :type "text/javascript"
                  :src "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"))
       (:body ,@body))))

(define-easy-handler (thym :uri "/thym") ()
  (standard-page (:title "Thym")
    (:h1 "Symbolic Computation")
    (:div :class "jumbotron"
      (:h2 "Type in an expression")             
      (:form :class "form-inline"
             :action "/calculate" :method "POST"
        (:input :class "form-control"
                :type "text" :id "expr" :name "expr")
        (:input :type "submit" :value "Evaluate!")))))

(define-easy-handler (calc :uri "/calculate") ()
  (standard-page ()
    (:p "Evaluation:")
    (str (conc "$$"
               (thym::write-latex
                (eval (parse (post-parameter "expr"))))
               "$$"))
    (:a :href "/thym" "Go back to thym")))
