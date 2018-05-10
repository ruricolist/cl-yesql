(defpackage :cl-yesql/sqlite-common
  (:use :cl :cl-yesql)
  (:import-from :cl-yesql/statement :parameter-var)
  (:export :query-string :var-offset))
(in-package :cl-yesql/sqlite-common)

(defun query-string (q)
  (with-output-to-string (s)
    (dolist (form (query-statement q))
      (if (stringp form)
          (write-string form s)
          (format s "?~a" (var-offset q form))))))
