(defpackage :cl-yesql/queryfile
  (:documentation "Parser for query files.")
  (:use :cl :alexandria :serapeum :esrap)
  (:shadow :comment :whitespace)
  (:shadowing-import-from :cl-yesql/defrule
    :defrule)
  (:import-from :cl-yesql/statement)
  (:export
   :queries
   :query
   :query-name
   :query-annotation
   :query-docstring
   :query-statement
   :copy-query
   :name
   :annotations
   :annotation))
(in-package :cl-yesql/queryfile)

(defconst annotations
  '(:rows :row :values :column :single :execute :last-id :setter))

(deftype annotation ()
  `(member ,@annotations))

(-> string->annotation (string) annotation)
(defun string->annotation (string)
  #.`(string-ecase string
       ,@(loop for annot in annotations
               for string = (string-downcase annot)
               collect `(,string ,annot))))

(defconstructor query
  (name string)
  (annotation annotation)
  (docstring string)
  (statement list))

(defun name-affix (name)
  (assure (or null annotation)
    (cond ((string$= "!" name) :execute)
          ((string$= "<!" name) :last-id)
          ((string^= "count-" name) :single)
          ((or (string$= "-p" name)
               (string$= "?" name))
           :single)
          (t nil))))

(defrule queries (and (* blank-line) (* query))
  (:function second))

(defconst no-docs
  "No docs.")

(defrule query (and name (? docstring) statement)
  (:destructure (name docstring statement)
                (destructuring-bind (name . annotation) name
                  (query name
                         (or annotation
                             (name-affix name)
                             :rows)
                         (or docstring no-docs)
                         (parse 'cl-yesql/statement:statement statement)))))

(defrule docstring (+ comment)
  (:lambda (comments)
    (trim-whitespace (string-join comments #\Newline))))

(defrule statement (and line (* (or line comment)))
  (:lambda (lines)
    (trim-whitespace (text (remove nil lines)))))

(defrule annotation (and (? whitespace)
                         "@" #.`(or ,@(mapcar #'string-downcase annotations)))
  (:lambda (args)
    (string->annotation (third args))))

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
          (annotation (third args)))
      (cons name annotation))))

(defrule comment
    (and (and (? whitespace)
              comment-marker
              (? whitespace))
         (! name-tag)
         (* (and non-whitespace (? whitespace)))
         newline)
  (:lambda (args)
    (trim-whitespace
     (text (rest args)))))

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
  (:text t))
