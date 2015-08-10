(defpackage #:symcl/functions
  (:use #:cl #:symcl #:symcl/core)
  #.(list* ':shadowing-import-from
           '#:symcl/core
           symcl::+symcl-common-lisp-shadowed-symbols+))
