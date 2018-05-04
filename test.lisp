(defpackage :cl-yesql/test
  (:use :cl :alexandria :serapeum :fiveam :cl-yesql)
  (:import-from :trivia :match)
  (:export :run-tests))
(in-package :cl-yesql/test)

(defun run-tests ()
  (run! 'cl-yesql))

(def-suite cl-yesql)
(in-suite cl-yesql)

(def users-by-country
  (trim-whitespace
   "
-- name: users-by-country @rows
-- Counts the users in a given country.
SELECT count(*) AS count
FROM user
WHERE country_code = :country_code"))

(def user-count
  (trim-whitespace "
-- name: user-count @single
-- Counts all the users.
SELECT count(*) AS count
FROM user"))

(def young-user-names-by-country
  (trim-whitespace "
-- name: young-user-names-by-country @column
SELECT name
FROM user
WHERE (
  country_code = ?
  OR
  country_code = ?
)
AND age < :max_age"))

(test parsing-finishes
  (finishes (parse-query users-by-country))
  (finishes (parse-query user-count))
  (finishes (parse-query young-user-names-by-country)))

(test docstrings
  "Check that docstrings are parsed correctly."
  (is
   (equal "Counts the users in a given country."
          (query-docstring
           (parse-query
            users-by-country))))

  (is
   (equal "Counts all the users."
          (query-docstring
           (parse-query
            user-count)))))

(test multi-line-docstrings
  "Check that multi-line docstrings are parsed correctly."
  (let ((query-string
          (trim-whitespace "
-- name: user-count @single
-- Counts all the users.
-- Hey, this database lets us use a table named user!
SELECT count(*) AS count FROM user")))
    (is (equal "Counts all the users.
Hey, this database lets us use a table named user!"
               (query-docstring
                (parse-query query-string))))))

(test annotations
  "Check that explicit annotations are parsed correctly."
  (is (eql :single
           (query-annotation
            (parse-query
             user-count))))

  (is (eql :rows
           (query-annotation
            (parse-query
             users-by-country))))

  (is (eql :column
           (query-annotation
            (parse-query
             young-user-names-by-country)))))

(test affixes
  "Check that affixes are parsed correctly."
  (is (eql :execute
           (query-annotation
            (parse-query
             (trim-whitespace
              "
-- name: spool!
INSERT INTO spool (link, url) VALUES (:link, :url) ON CONFLICT DO NOTHING")))))

  (is (eql :single
           (query-annotation
            (parse-query
             (trim-whitespace
              "
-- name: count-items-stored
SELECT reltuples FROM pg_class WHERE relname = 'item'")))))

  (is (eql :single
           (query-annotation
            (parse-query
             (trim-whitespace
              "
-- name: known-link?
SELECT TRUE
 FROM links
 WHERE src = :src"))))))

(test keyword-args
  "Test keyword args."
  (let* ((query (parse-query users-by-country))
         (args (query-args query)))
    (is-true
     (match args
       ((list '&key (list _ (list 'required-argument _)))
        t)))))

(test positional-and-keyword-args
  "Mixing positional and keyword args should work."
  (let* ((query (parse-query young-user-names-by-country))
         (args (query-args query)))
    (is-true
     (match args
       ((list _ _ '&key (list _ (list 'required-argument _)))
        t)))))

(test same-arg-twice
  "The same named arg used twice should only result in one formal."
  (let* ((query-string "
-- name: untagged-feeds @column
select feed from subscription
where user_id = :user
    except (select feed from feed_tag
    where user_id = :user)")
         (query (parse-query (trim-whitespace query-string)))
         (args (query-args query)))
    (is-true
     (match args
       ((list '&key _)
        t)))))

(test big-pg-queries
  "Check that some fancy postgres queries are parsed correctly."
  (finishes
    (parse-query
     (trim-whitespace "
-- name: table-sizes
SELECT relname AS \"relation\",
    pg_size_pretty(pg_total_relation_size(C.oid)) AS \"total_size\"
FROM pg_class C
    LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace)
WHERE nspname NOT IN ('pg_catalog', 'information_schema')
    AND C.relkind <> 'i'
    AND C.relkind <> 'S'
    AND nspname !~ '^pg_toast'
ORDER BY pg_total_relation_size(C.oid) DESC ")))

  (finishes
    (parse-query
     (trim-whitespace "
-- name: table-indexes
SELECT c2.relname, i.indisprimary, i.indisunique,
    i.indisclustered, i.indisvalid,
    pg_catalog.pg_get_indexdef(i.indexrelid, 0, true),
    pg_catalog.pg_get_constraintdef(con.oid, true),
    contype, condeferrable, condeferred, c2.reltablespace
FROM pg_catalog.pg_class c, pg_catalog.pg_class c2,
    pg_catalog.pg_index i
    LEFT JOIN pg_catalog.pg_constraint con
    ON (conrelid = i.indrelid AND conindid = i.indexrelid AND contype IN ('p','u','x'))
WHERE c.oid = :oid AND c.oid = i.indrelid AND i.indexrelid = c2.oid
ORDER BY i.indisprimary DESC, i.indisunique DESC, c2.relname"))))
