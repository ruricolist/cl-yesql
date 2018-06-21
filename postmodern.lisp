(defpackage #:cl-yesql/postmodern
  (:use #:cl #:alexandria #:serapeum #:cl-yesql)
  (:shadowing-import-from #:cl-yesql #:import)
  (:import-from #:pomo #:execute #:prepare #:*database*)
  (:shadowing-import-from #:ppcre #:scan)
  (:shadowing-import-from #:pomo #:query)
  (:shadowing-import-from #:cl-yesql/lang
    #:read-module
    #:module-progn)
  (:export #:yesql-postmodern #:static-exports
           #:read-module #:module-progn))
(in-package #:cl-yesql/postmodern)

;;; TODO The limitations of the Yesql language are iteration and
;;; Postmodern DAOs. Iteration should be doable with a postmodern/iter
;;; language, but I'm not sure if DAOs can be made to fit the Yesql
;;; paradigm.

(defun check-connection ()
  (loop until *database* do
    (cerror "Check again"
            "There is no database connection.")))

(defmacro defquery (name args &body (docstring query))
  `(defun ,name ,args
     ,docstring
     (check-connection)
     ,(build-query-tree
       query
       (lambda (q)
         (query-body q)))))

(defun static-exports (source)
  (yesql-static-exports source))

(defun query-body (query)
  (let ((annot (query-annotation query)))
    (ecase-of annotation annot
      ((:single :row :rows :column)
       (simple-query-body query annot))
      ((:execute)
       (simple-query-body query :none))
      ((:values)
       `(values-list ,(simple-query-body query :row)))
      (:last-id
       (error "Auto-id queries not supported for Postgres.~%Consider `INSERT ... RETURNING` instead.")))))

(defun simple-query-body (query style)
  (let ((vars (query-vars query))
        (statement (query-string query)))
    `(funcall ,(statement-fn-form statement style)
              ,@vars)))

(defun statement-fn-form (statement style)
  (if (string-prefix-p "create" statement)
      (if (eql style :none)
          `(lambda ()
             (pomo:execute ,statement))
          (error "A DDL statement cannot return."))
      `(load-time-value
        (pomo:prepare ,statement ,style))))

(defun sanity-check-fragment (fragment)
  (when (scan "\\$\\d+" fragment)
    (simple-style-warning "Suspicious occurrence of $n in a Postgres ~
      statement: ~s.~%Yesql uses ? for positional arguments."
                          fragment)))

(defun query-string (q)
  (with-output-to-string (s)
    (dolist (form (query-statement q))
      (if (stringp form)
          (progn
            (sanity-check-fragment form)
            (write-string form s))
          (format s "$~a" (var-offset q form))))))


