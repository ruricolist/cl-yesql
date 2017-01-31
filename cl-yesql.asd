;;;; cl-yesql.asd

(asdf:defsystem #:cl-yesql
  :description "Common Lisp library for using SQL."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:cl-yesql/cl-yesql))

(register-system-packages "postmodern" '(:postmodern :pomo))
(register-system-packages "cl-ppcre" '(:cl-ppcre :ppcre))

