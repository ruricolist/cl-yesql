(defpackage #:cl-yesql/postmodern
  (:use #:cl #:alexandria #:serapeum #:cl-yesql)
  (:import-from #:overlord #:simple-module)
  (:import-from #:pomo #:execute #:prepare #:*database*)
  (:shadowing-import-from #:ppcre #:scan)
  (:shadowing-import-from #:pomo #:query)
  (:shadow #:read-module)
  (:export #:yesql-postmodern #:static-exports
           #:read-module #:module-progn))
(in-package #:cl-yesql/postmodern)

(defun check-connection ()
  (loop until *database* do
    (cerror "Check again"
            "There is no database connection.")))

(defun read-module (source stream)
  `(module-progn
     ,@(cl-yesql:yesql-reader source stream)))

(defmacro module-progn (&body forms)
  (let ((exports (mapcar (op `(function ,(second _))) forms)))
    `(simple-module ,exports
       ,@forms)))

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
    `(funcall (load-time-value (pomo:prepare ,statement ,style))
              ,@vars)))

(defun sanity-check-fragment (fragment)
  (when (scan "\\$\\d+" fragment)
    (simple-style-warning "Suspicious occurrence of $n in a Postgres ~
      statement.~%Yesql uses ? for positional arguments.")))

(defun query-string (q)
  (with-output-to-string (s)
    (dolist (form (query-statement q))
      (if (stringp form)
          (progn
            (sanity-check-fragment form)
            (write-string form s))
          (format s "$~a" (var-offset q form))))))


