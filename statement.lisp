(defpackage :cl-yesql/statement
  (:documentation "Parser for statements.")
  (:use :cl :alexandria :serapeum :esrap)
  (:shadow :comment :whitespace :string)
  (:shadowing-import-from :cl-yesql/defrule
    :defrule)
  (:import-from :trivia :match)
  (:export
   :placeholder
   :statement
   :lispify-sql-id
   :parameter
   :parameter-var
   :parameter-whitelist
   :positional-arg?))
(in-package :cl-yesql/statement)

(defunit placeholder)

(defconstructor parameter
  (var (or symbol placeholder))
  (whitelist list))

(defconst positional-args
  (loop for i from 0 to 50
        collect (intern (fmt "?~a" i) #.*package*)))

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

(defun lispify-sql-id (id &key (package *package*))
  (~> id
      (substitute #\- #\_ _)
      string-upcase
      (intern package)))

(defrule statement
    (and substatement (* (and parameter substatement)))
  (:lambda (tree)
    (~> tree
        flatten
        handle-placeholders)))

(defrule substatement
    (* (or (or (+ (not (or #\? #\: #\')))
               "::")
           string))
  (:text t))

(defrule whitelist
    (and (and #\{ (* whitespace))
         (* whitelist-item)
         (and (* whitespace) #\}))
  (:function second))

(defrule whitelist-item
    (and (* whitespace)
         (+ (not (or whitespace #\, #\})))
         (and (* whitespace) (? #\,)))
  (:lambda (m)
    (text (second m))))

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
    (or whitelist-parameter simple-parameter))

(defrule simple-parameter
    (or placeholder-parameter
        named-parameter))

(defrule whitelist-parameter
    (and simple-parameter (? whitelist))
  (:lambda (args)
    (apply #'parameter args)))

(defrule placeholder-parameter "?"
  (:constant placeholder))

(defrule named-parameter
    (and ":"
         (+ (not
             #.`(or whitespace newline
                    ,@(coerce "{},\"':&;()|=+\-*%/\\<>^" 'list)))))
  (:lambda (args)
    (lispify-sql-id (string-upcase (text (second args))))))

(defrule whitespace
    (+ (or #\Space #\Tab)))

(defrule newline (or #\Newline (and #\Return #\Newline))
  (:text t))
