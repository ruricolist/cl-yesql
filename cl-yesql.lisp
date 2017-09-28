;;;; cl-yesql.lisp

(defpackage #:cl-yesql
  (:use #:cl
    #:alexandria
    #:serapeum
    #:cl-yesql/defgrammar)
  (:nicknames #:yesql)
  (:import-from #:overlord)
  (:export
   #:parse-query
   #:parse-queries

   #:query
   #:query-name #:query-id
   #:annotation #:query-annotation
   #:query-docstring
   #:query-statement
   #:query-vars #:query-args

   #:yesql-static-exports

   #:yesql

   #:yesql-reader #:read-module))

(defpackage #:cl-yesql-user
  (:use))

(in-package #:cl-yesql)

;;; "cl-yesql" goes here. Hacks and glory await!

(eval-and-compile
  (def annotations
    '(:rows :row :values :column :single :execute :last-id)))

(deftype annotation ()
  `(member ,@annotations))

(defclass query ()
  ((name :initarg :name :type string :reader query-name)
   (annotation :initarg :annotation :type annotation :reader query-annotation)
   (docstring :type string :accessor query-docstring)
   (statement :type (or string list) :initarg :statement
              :accessor query-statement)
   (vars :type list :accessor query-vars))
  (:default-initargs
   :statement (required-argument 'statement)
   :name (required-argument 'name)
   :annotation :rows))

(defmethod slot-unbound (class (self query) (slot (eql 'vars)))
  (declare (ignore class))
  (setf (slot-value self 'vars)
        (statement-vars (query-statement self))))

(defmethod query-statement :around ((self query))
  (let ((statement (call-next-method)))
    (etypecase statement
      (string (setf (query-statement self) (parse-statement statement)))
      (list statement))))

(defvar *positional-args*
  (loop for i from 0 to 50 collect (intern (fmt "?~a" i))))

(defun positional-arg? (arg)
  (member arg *positional-args*))

(defmethod statement-vars ((statement list))
  (mvlet* ((symbols (filter #'symbolp statement))
           (positional keywords (partition #'positional-arg? symbols)))
    (assert (equal positional (nub positional)))
    (append positional (nub keywords))))

(defmethod initialize-instance :after ((self query)
                                       &key docstring
                                       &allow-other-keys)
  (setf (query-docstring self) (or docstring "No docs.")))

(defmethod print-object ((self query) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (name annotation statement) self
      (format stream "~s ~s ~s" name annotation statement))))

(defun query-id (q)
  (lispify-sql-id (query-name q)))

(defun make-query (&rest args)
  (apply #'make 'query args))

(defgrammar queries
  (defrule queries (and (* blank-line) (* query))
    (:function second))

  (defrule query (and name (? docstring) statement)
    (:destructure (name docstring statement)
                  (destructuring-bind (name . annotation) name
                    (make-query
                     :annotation annotation
                     :name (text name)
                     :docstring docstring
                     :statement statement))))

  (defrule docstring (+ comment)
    (:lambda (comments)
      (trim-whitespace (text comments))))

  (defrule statement (and line (* (or line comment)))
    (:lambda (lines)
      (trim-whitespace (text (remove nil lines)))))

  (defrule annotation (and (? whitespace)
                           "@" #.`(or ,@(mapcar #'string-downcase annotations)))
    (:lambda (args)
      (string-ecase (third args)
        ("single" :single)
        ("row" :row)
        ("rows" :rows)
        ("column" :column)
        ("execute" :execute)
        ("last-id" :last-id))))

  (defrule name
      (and (and (? whitespace)
                comment-marker
                (? whitespace)
                name-tag
                (? whitespace))
           non-whitespace
           (? annotation)
           (and (? whitespace) newline))
    (:lambda (args)
      (let ((name (second args))
            (annotation (or (third args) :rows)))
        (cons name annotation))))

  (defrule comment
      (and (and (? whitespace)
                comment-marker
                (? whitespace))
           (! name-tag)
           (* (and non-whitespace (? whitespace)))
           newline)
    (:constant nil))

  (defrule line
      (and (? whitespace)
           (! comment-marker)
           (* (and non-whitespace (? whitespace)))
           newline)
    (:text t))

  (defrule comment-marker "--")
  (defrule name-tag "name:")

  (defrule blank-line (and (* whitespace) newline))
  (defrule any (+ (or whitespace non-whitespace))
    (:text t))
  (defrule newline (or #\Newline (and #\Return #\Newline))
    (:text t))
  (defrule whitespace (+ (or #\Space #\Tab))
    (:text t))

  (defrule non-whitespace (+ (not (or whitespace newline)))
    (:text t)))

(defgrammar statement

  (defrule statement
      (and substatement (* (and parameter substatement))))

  (defrule substatement
      (* (or (or (+ (not (or #\? #\: #\')))
                 "::")
             string))
    (:text t))

  (defrule string
      (and string-delimiter
           (* (or string-normal string-special))
           string-delimiter)
    (:lambda (s)
      (fmt "'~a'" (text s))))
  (defrule string-delimiter "'"
    (:constant nil))
  (defrule string-normal
      (not (or #\' #\\)))
  (defrule string-special
      (and #\\ character)
    (:function second))

  (defrule parameter
      (or placeholder-parameter
          named-parameter))

  (defrule placeholder-parameter "?"
    (:constant ':?))

  (defrule named-parameter
      (and ":"
           (+ (not
               #.`(or whitespace newline
                      ,@(coerce ",\"':&;()|=+\-*%/\\<>^" 'list)))))
    (:lambda (args)
      (lispify-sql-id (string-upcase (text (second args))))))

  (defrule whitespace
      (+ (or #\Space #\Tab)))

  (defrule newline (or #\Newline (and #\Return #\Newline))
    (:text t)))

(defmethod statement-vars ((s string))
  (.parse statement 'statement s))

(defun parse-statement (s)
  (let* ((statement (flatten (.parse statement 'statement s)))
         (positional *positional-args*))
    (loop for part in statement
          if (eql part :?)
            collect (pop positional)
          else collect part)))

(defun print-sql (x s)
  (if (listp x)
      (loop for (each . more?) on x
            do (print-sql each s)
               (when more?
                 (write-string ", " s)))
      (prin1 x s)))

(defmethod parse-query ((s string))
  (.parse queries 'query (ensure-trailing-newline s)))

(defmethod parse-query ((p pathname))
  (parse-query (read-file-into-string p)))

(defun parse-queries (s)
  (let ((*package* (find-package :cl-yesql-user)))
    (etypecase s
      (string
       (.parse queries 'queries (ensure-trailing-newline s)))
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

(defun lispify-sql-id (id &key (package *package*))
  (~> id
      (substitute #\- #\_ _)
      string-upcase
      (intern package)))

(defun query-args (q)
  (mvlet* ((positional keyword (partition #'positional-arg? (query-vars q)))
           ;; Keyword arguments are not optional.
           (keyword
            (mapcar (op `(,_1 (required-argument ',_1))) keyword))
           (args (append positional (cons '&key keyword))))
    (assert (equal args (nub args)))
    args))

(defun query-affix (q)
  (let ((name (query-name q)))
    (cond ((string$= "!" name) :execute)
          ((string$= "<!" name) :last-id)
          ((string^= "count-" name) :single)
          ((or (string$= "-p" name)
               (string$= "?" name))
           :single)
          (t nil))))

(defmethod query-annotation :around ((self query))
  (or (query-affix self) (call-next-method)))

(defun yesql-static-exports (file)
  #+ () (mapcar #'query-id (parse-queries file))
  ;; Should this just be a regex?
  (with-input-from-file (in file)
    (loop for line = (read-line in nil nil)
          while line
          for name = (ignore-errors
                      (car
                       (.parse queries 'name
                               (concat line #.(string #\Newline)))))
          when name
            collect (lispify-sql-id name :package :keyword))))
