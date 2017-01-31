(defpackage :cl-yesql/sqlite-common
  (:use :cl :cl-yesql)
  (:export :query-string :var-offset))
(in-package :cl-yesql/sqlite-common)

(defun var-offset (q var)
  (1+ (position var (query-vars q))))

(defun query-string (q)
  (with-output-to-string (s)
    (dolist (form (query-statement q))
      (if (stringp form)
          (write-string form s)
          (format s "?~a" (var-offset q form))))))
