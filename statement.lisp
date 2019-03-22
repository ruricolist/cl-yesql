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
   :positional-arg?
   :too-many-placeholders))
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

(defun handle-placeholders (orig-statement)
  (nlet rec ((statement orig-statement)
             (positionals positional-args)
             (acc '()))
    (cond ((endp statement)
           (nreverse acc))
          ((endp positionals)
           (error 'too-many-placeholders
                  :statement orig-statement))
          ((and (typep (first statement) 'parameter)
                (typep (parameter-var (first statement))
                       'placeholder))
           (rec (rest statement)
                (rest positionals)
                (cons (parameter (first positionals)
                                 (parameter-whitelist
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
