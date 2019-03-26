;;;; cl-yesql.lisp

(defpackage #:cl-yesql
  (:use #:cl #:alexandria #:serapeum
    #:cl-yesql/queryfile
    #:cl-yesql/statement)
  (:nicknames #:yesql)
  (:shadowing-import-from :vernacular
    #:import)
  (:import-from #:trivia
    #:match)
  (:import-from #:esrap
    #:parse)
  (:export
   #:import

   #:parse-query
   #:parse-queries

   #:query
   #:query-name #:query-id
   #:annotation #:query-annotation
   #:query-docstring
   #:query-statement
   #:query-vars #:var-offset #:query-args
   #:build-query-tree

   #:yesql-static-exports

   #:yesql

   #:yesql-reader #:read-module
   #:need))

(defpackage #:cl-yesql-user
  (:use))

(in-package #:cl-yesql)

;;; TODO One more feature I want: the ability to reference a previous
;;; query in a query. That way you could easily create trivial
;;; variations on a base query, e.g. SELECT COUNT(*) FROM $query or
;;; SELECT EXISTS $query. Perhaps this could be done by building on
;;; the whitelist feature with an implicit prefix and suffix argument.

(defun query-vars (query)
  (statement-vars (query-statement query)))

(defun var-offset (q param)
  (let ((var (parameter-var param)))
    (1+ (position var (query-vars q)))))

(defun statement-vars (statement)
  (mvlet* ((parameters (filter (of-type 'parameter) statement))
           (symbols (mapcar #'parameter-var parameters))
           (positional keywords (partition #'positional-arg? symbols)))
    (assert (equal positional (nub positional)))
    (append positional (nub keywords))))

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
  (parse 'query (ensure-trailing-newline s)))

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
  (let ((defquery (vernacular:reintern 'defquery)))
    (loop for query in (parse-queries stream)
          collect `(,defquery ,(query-id query) ,(query-args query)
                     ,(query-docstring query)
                     ,query))))

(defun read-module (source stream)
  (vernacular:with-meta-language (source stream)
    (yesql-reader source stream)))

(defun ensure-trailing-newline (s)
  (let ((nl #.(string #\Newline)))
    (if (string$= nl s)
        s
        (concat s nl))))

(defsubst need (arg)
  "Shorthand for alexandria:required-argument."
  (required-argument arg))

(defun query-args (q)
  (mvlet* ((positional keywords (partition #'positional-arg? (query-vars q)))
           ;; Keyword arguments are not optional. In particular,
           ;; backends differ in how they treat `nil': e.g. sqlite
           ;; treats it as NULL, but cl-postgres treats it as FALSE.
           (keywords
            (loop for var in keywords
                  collect `(,var (need ,(make-keyword var)))))
           (args (append positional (cons '&key keywords))))
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

(defcondition string-not-in-whitelist (error)
  ((string :initarg :string :type string)
   (whitelist :initarg :whitelist :type whitelist))
  (:report (lambda (c s)
             (with-slots (string whitelist) c
               (format s "String ~s is not in whitelist ~s."
                       string whitelist)))))

(defun invalid-string (string whitelist)
  (error 'string-not-in-whitelist
         :string string
         :whitelist whitelist))

(defun check-query-expanded (query)
  (null (query-whitelist-parameters query)))

(defun query-whitelist-parameters (query)
  (filter (conjoin (of-type 'parameter)
                   #'has-whitelist?)
          (query-statement query)))

(defun has-whitelist? (x)
  (not (null (parameter-whitelist x))))

(defun build-query-tree (query fun)
  "Call FUN on each concrete expansion of QUERY.

E.g., if QUERY has single parameter with a whitelist with three
possible expansions, then FUN will be called on each of the three
possible versions of QUERY. If there is a second parameter with two
expansions, then FUN will be called on each of six (=2*3) possible
expansions."
  (fbindrec (fun
             (rec
              (lambda (query params)
                (if (null params) (fun query)
                    (let* ((param (first params))
                           (var (parameter-var param))
                           (whitelist (parameter-whitelist param)))
                      `(string-case ,var
                         ,@(loop for string in whitelist
                                 for old-stat = (query-statement query)
                                 for new-stat = (substitute string param old-stat :count 1)
                                 for q = (copy-query query :statement new-stat)
                                 collect `(,string ,(rec q (rest params))))
                         (t (invalid-string ,var ',whitelist))))))))
    (rec query (query-whitelist-parameters query))))
