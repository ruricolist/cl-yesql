(defpackage :cl-yesql/defrule
  (:use :cl)
  (:import-from :esrap)
  (:export :defrule))
(in-package :cl-yesql/defrule)

(defmacro defrule (symbol expr &body options)
  "Don't permit rules using inherited symbols."
  (assert (eql (symbol-package symbol) *package*)
          () "Symbol ~a is not in current package."
          symbol)
  `(esrap:defrule ,symbol ,expr
     ,@options))
