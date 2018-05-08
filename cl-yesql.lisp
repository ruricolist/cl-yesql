;;;; cl-yesql.lisp

(defpackage #:cl-yesql
  (:use #:cl #:alexandria #:serapeum
    #:cl-yesql/queryfile
    #:cl-yesql/statement)
  (:nicknames #:yesql)
  (:import-from #:overlord)
  (:import-from #:esrap
    #:parse)
  (:export
   #:parse-query
   #:parse-queries

   #:query
   #:query-name #:query-id
   #:annotation #:query-annotation
   #:query-docstring
   #:query-statement
   #:query-vars #:query-args
   #:query-dispatch

   #:yesql-static-exports

   #:yesql

   #:yesql-reader #:read-module))

(defpackage #:cl-yesql-user
  (:use))

(in-package #:cl-yesql)

;;; "cl-yesql" goes here. Hacks and glory await!

(defun query-vars (query)
  (statement-vars (query-statement query)))

(defconst positional-args
  (loop for i from 0 to 50
        collect (intern (fmt "?~a" i) :cl-yesql)))

(defun positional-arg? (arg)
  (memq arg positional-args))

(defun handle-placeholders (statement)
  (let ((positionals positional-args))
    (mapcar
     (lambda (elt)
       (match elt
         ((parameter (and _ (type placeholder)) whitelist)
          (let ((var (pop positionals)))
            (parameter var whitelist)))
         (otherwise elt)))
     statement)))

(defgeneric statement-vars (s)
  (:method ((s string))
    (statement-vars (parse 'statement s)))
  (:method ((statement list))
    (mvlet* ((parameters (filter (of-type 'parameter) statement))
             (symbols (mapcar #'parameter-var parameters))
             (positional keywords (partition #'positional-arg? symbols)))
      (assert (equal positional (nub positional)))
      (append positional (nub keywords)))))

(defconst no-docs "No docs.")

(defun query-id (q)
  (lispify-sql-id (query-name q)))

(defun print-sql (x s)
  (if (listp x)
      (loop for (each . more?) on x
            do (print-sql each s)
               (when more?
                 (write-string ", " s)))
      (prin1 x s)))

(defmethod parse-query ((s string))
  (let* ((query (parse 'query (ensure-trailing-newline s)))
         (statement (query-statement query)))
    (copy-query query
                :statement
                (handle-placeholders
                 (parse 'statement statement)))))

(defmethod parse-query ((p pathname))
  (parse-query (read-file-into-string p)))

(defun parse-queries (s)
  (let ((*package* (find-package :cl-yesql-user)))
    (etypecase s
      (string
       (parse 'queries (ensure-trailing-newline s)))
      (pathname
       (parse-queries (read-file-into-string s)))
      (stream
       (assert (input-stream-p s))
       (parse-queries (read-stream-content-into-string s))))))

(defun yesql-reader (path stream)
  (declare (ignore path))
  (let ((defquery (overlord:reintern 'defquery)))
    (loop for query in (parse-queries stream)
          collect `(,defquery ,(query-id query) ,(query-args query)
                     ,(query-docstring query)
                     ,query))))

(defun read-module (source stream)
  (overlord:with-meta-language (source stream)
    (yesql-reader source stream)))

(defun ensure-trailing-newline (s)
  (let ((nl #.(string #\Newline)))
    (if (string$= nl s)
        s
        (concat s nl))))

(defun query-args (q)
  (mvlet* ((positional keyword (partition #'positional-arg? (query-vars q)))
           ;; Keyword arguments are not optional.
           (keyword
            (mapcar (op `(,_1 (required-argument ',_1))) keyword))
           (args (append positional (cons '&key keyword))))
    (assert (equal args (nub args)))
    args))

(defun yesql-static-exports (file)
  #+ () (mapcar #'query-id (parse-queries file))
  ;; Should this just be a regex?
  (with-input-from-file (in file)
    (loop for line = (read-line in nil nil)
          while line
          for name = (ignore-errors
                      (car
                       (parse 'name (concat line #.(string #\Newline)))))
          when name
            collect (lispify-sql-id name :package :keyword))))

;; (defmacro query-whitelist-dispatch (query macro)
;;   (expand-query-whitelist-dispatch query macro))

;; (defun expand-query-whitelist-dispatch (query macro)
;;   (let* ((statement (query-statement query))
;;          (parameters (filter (of-type 'parameter) statement)))
;;     (if (every (op (null (parameter-whitelist _))) parameters)
;;         `(,macro ,query)
;;         (let* ((param (first parameters))
;;                (var (parameter-var param)))
;;           `(string-case ,var
;;              ,@(loop for string in (parameter-whitelist var)
;;                      collect `(,string
;;                                ,(expand-query-whitelist-dispatch
;;                                  (copy-query query
;;                                              :statement (substitute string param statement :count 1))
;;                                  macro))))))))
