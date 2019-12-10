(defpackage :cl-yesql/sqlite-prepared
  (:use :cl
    :alexandria :serapeum
    :cl-yesql :cl-yesql/sqlite-common)
  (:shadowing-import-from :cl-yesql :import)
  (:import-from :sqlite)
  (:shadowing-import-from :cl-yesql/lang
    :read-module
    :module-progn)
  (:export
   :yesql-sqlite-prepared
   :with-prepared-statement
   :read-module :module-progn
   :static-exports))
(in-package :cl-yesql/sqlite-prepared)

(defun static-exports (file)
  (yesql-static-exports file))

(defmacro defquery (name args &body (docstring query))
  (declare (ignore args))
  `(defun ,name ()
     ,docstring
     ,(build-query-tree
       query
       (lambda (q)
         `(values ,(query-string q)
                  ,(query-thunk q))))))

(defvar *prepared-query*)
(defvar *prepared-db*)

(defun call/sqlite-statement (statement db thunk)
  (let ((prepared (sqlite:prepare-statement db statement)))
    (unwind-protect
         (funcall thunk prepared)
      (sqlite:finalize-statement prepared))))

(defmacro with-sqlite-statement ((var handle) statement &body body)
  (with-thunk (body var)
    `(call/sqlite-statement ,statement ,handle ,body)))

(defmacro with-prepared-statement ((fn query db) &body body)
  (once-only (db query)
    (with-gensyms (statement thunk str)
      `(multiple-value-bind (,str ,thunk) (funcall ,query)
         (with-sqlite-statement (,statement ,db) ,str
           (fbind ((,fn (lambda (&rest args)
                          (apply ,thunk ,statement args))))
             ,@body))))))

(defun query-thunk (q)
  (let ((vars (query-vars q))
        (args (query-args q)))
    (with-gensyms (statement)
      `(lambda (,statement ,@args)
         ,@(loop for var in vars
                 for offset from 1
                 collect `(sqlite:bind-parameter ,statement ,offset ,var))
         (sqlite:step-statement ,statement)
         (sqlite:reset-statement ,statement)))))
