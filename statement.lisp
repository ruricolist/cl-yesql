(defpackage :cl-yesql/statement
  (:documentation "Parser for statements.")
  (:use :cl :alexandria :serapeum :esrap)
  (:shadow :comment :whitespace :string)
  (:shadowing-import-from :cl-yesql/defrule
    :defrule)
  (:export
   :statement
   :lispify-sql-id))
(in-package :cl-yesql/statement)

(defun lispify-sql-id (id &key (package *package*))
  (~> id
      (substitute #\- #\_ _)
      string-upcase
      (intern package)))

(defrule statement
    (and substatement (* (and parameter substatement)))
  (:lambda (s)
    (flatten s)))

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
  (:text t))
