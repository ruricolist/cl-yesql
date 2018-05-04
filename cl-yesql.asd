;;;; cl-yesql.asd

(asdf:defsystem #:cl-yesql
  :description "Common Lisp library for using SQL."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on ("cl-yesql/cl-yesql")
  :in-order-to ((test-op (test-op "cl-yesql/test")))
  :perform (test-op (o c) (symbol-call :cl-yesql/test :run-tests)))

(register-system-packages "postmodern" '(:postmodern :pomo))
(register-system-packages "cl-ppcre" '(:cl-ppcre :ppcre))

