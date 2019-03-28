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
   :positional?
   :too-many-placeholders))
(in-package :cl-yesql/statement)

(defclass parameter ()
  ((whitelist
    :type list
    :initarg :whitelist
    :reader parameter-whitelist))
  (:default-initargs :whitelist nil))

(defclass named-parameter (parameter)
  ((var
    :type symbol
    :initarg :var
    :initform (required-argument :var)
    :reader parameter-var)))

(defmethod print-object ((self named-parameter) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (parameter-var self))))

(defclass placeholder-parameter (parameter)
  ())

(defclass anonymous-placeholder (placeholder-parameter)
  ())

(defclass named-placeholder (placeholder-parameter named-parameter)
  ())

(defclass keyword-parameter (named-parameter)
  ())

(defgeneric positional? (param)
  (:method ((param parameter))
    nil)
  (:method ((param placeholder-parameter))
    t))

(defconst positional-args
  (loop for i from 0 to 50
        collect (intern (fmt "?~a" i) #.*package*)))

(defun handle-placeholders (orig-statement)
  (nlet rec ((statement orig-statement)
             (positionals positional-args)
             (acc '()))
    (cond ((endp statement)
           (nreverse acc))
          ((endp positionals)
           (error 'too-many-placeholders
                  :statement orig-statement))
          ((and (typep (first statement) 'anonymous-placeholder))
           (rec (rest statement)
                (rest positionals)
                (cons (make 'named-placeholder
                            :var (first positionals)
                            :whitelist (parameter-whitelist
                                        (first statement)))
                      acc)))
          (t
           (rec (rest statement)
                positionals
                (cons (first statement) acc))))))

(defun lispify-sql-id (id &key (package *package*))
  (~> id
      (substitute #\- #\_ _)
      string-upcase
      (intern package)))

(defcondition too-many-placeholders (error)
  ((statement :initarg :statement))
  (:report (lambda (c s)
             (with-slots (statement) c
               (format s "Too many (>~a) positional arguments in ~a."
                       (length positional-args)
                       statement)))))

(defrule statement
    (and substatement (* (and parameter substatement)))
  (:lambda (tree)
    (~>> tree
         flatten
         handle-placeholders
         (remove-if (conjoin #'stringp #'emptyp)))))

(defrule substatement
    (* (or (or (+ (not (or #\? #\: #\' comment-start)))
               "::")
           comment
           string))
  (:text t))

(defrule whitelist
    (and (and #\{ (* whitespace))
         (* whitelist-item)
         (and (* whitespace) #\}))
  (:lambda (args)
    (let* ((whitelist (second args))
           (uniqs (remove-duplicates whitelist :test #'string=)))
      (if (length= whitelist uniqs) whitelist
          (progn
            (cerror "Drop duplicated items"
                    "Duplicate items in whitelist: ~a" whitelist)
            uniqs)))))

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
    (and simple-parameter (? whitelist))
  (:lambda (args)
    (apply #'make
           (append (first args)
                   (list :whitelist (second args))))))

(defrule simple-parameter
    (or placeholder-parameter
        keyword-parameter))

(defrule placeholder-parameter "?"
  (:lambda (args)
    (declare (ignore args))
    (list 'anonymous-placeholder)))

(defrule keyword-parameter
    (and ":" parameter-name)
  (:lambda (args)
    (list 'keyword-parameter
          :var (second args))))

(defrule parameter-name
    (+ (not
        #.`(or whitespace newline
               ,@(coerce "{},\"':&;()|=+\-*%/\\<>^" 'list))))
  (:lambda (args)
    (~> args
        text
        string-upcase
        lispify-sql-id)))

(defrule single-line-comment-start "--")

(defrule single-line-comment
    (and single-line-comment-start
         (* (not newline))
         (? newline))
  (:lambda (args)
    ;; When possible, preserve single-line comments by rewriting them
    ;; as multi-line comments.
    (let ((comment-text (second args)))
      ;; Perversity.
      (if (search "/*" comment-text)
          nil
          (list "/*" (text (second args)) "*/")))))

(defrule multi-line-comment-start "/*")

(defrule multi-line-comment
    (and multi-line-comment-start
         (* (not "*/"))
         "*/")
  (:lambda (args)
    (list "/*" (text (second args)) "*/")))

(defrule comment-start (or single-line-comment-start multi-line-comment-start))

(defrule comment (or single-line-comment multi-line-comment))

(defrule whitespace
    (+ (or #\Space #\Tab)))

(defrule newline (or #\Newline (and #\Return #\Newline))
  (:text t))
