(defpackage #:cl-yesql/sqlite
  (:use
    #:cl #:alexandria #:serapeum #:cl-yesql #:sqlite
    :cl-yesql/sqlite-common)
  (:import-from #:overlord #:simple-module)
  (:shadow #:read-module)
  (:export #:yesql-sqlite #:read-module #:module-progn #:static-exports))
(in-package #:cl-yesql/sqlite)

(defun read-module (source stream)
  `(module-progn
     ,@(yesql:yesql-reader source stream)))

(defmacro module-progn (&body forms)
  `(simple-module ,(mapcar (op `(function ,(second _))) forms)
     ,@forms))

(defmacro defquery (name args &body (docstring query))
  (with-gensyms (db)
    `(defun ,name (,db ,@args)
       ,docstring
       ,(build-query-tree
         query
         (lambda (q)
           (query-body db q))))))

(defun static-exports (source)
  (yesql-static-exports source))

;;; Although SQLite supports named arguments, we don't use them, so we
;;; don't have to worry about escaping. It should be straightforward
;;; to switch if that turns out to be a mistake.

(defun query-body (db q)
  (let* ((string (query-string q))
         (vars   (query-vars q))
         (args `(,db ,string ,@vars)))
    (ecase-of annotation (query-annotation q)
      (:rows `(execute-to-list ,@args))
      (:execute `(execute-non-query ,@args))
      (:last-id
       `(progn
          (execute-non-query ,@args)
          (last-insert-rowid ,db)))
      (:values `(execute-one-row-m-v ,@args))
      (:row `(multiple-value-list (execute-one-row-m-v ,@args)))
      (:column `(execute-to-column ,@args))
      (:single `(execute-single ,@args)))))

(defun execute-to-column (db sql &rest parameters)
  (declare (dynamic-extent parameters))
  (sqlite::with-prepared-statement stmt (db sql parameters)
    (loop while (step-statement stmt)
          collect (statement-column-value stmt 0))))
