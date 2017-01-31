(defpackage #:cl-yesql/defgrammar
  (:use #:cl #:alexandria #:serapeum #:esrap)
  (:import-from #:cl-yesql/esrap-user)
  (:export #:grammar #:defgrammar #:defrule #:? #:! #:.parse #:text #:.trace))
(in-package #:cl-yesql/defgrammar)

(defmacro with-grammar ((name expr) &body body)
  `(let* ((,name ,expr)
          (esrap::*rules* ,name))
     ,@body))

(defun .parse (grammar rule string &rest args)
  (with-grammar (g grammar)
    (apply #'parse rule string args)))

(defun .trace (grammar rule &rest args)
  (with-grammar (g grammar)
    (apply #'trace-rule rule args)))

(defun .describe (grammar rule)
  (with-grammar (g grammar)
    (describe-grammar rule)))

(defun make-grammar ()
  (make-hash-table))

(defmacro defgrammar (name &body body)
  `(def ,name
     (with-grammar (,name (make-grammar))
       ,@body
       ,name)))
