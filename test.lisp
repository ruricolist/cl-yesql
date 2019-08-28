(defpackage :cl-yesql/test
  (:use :cl :alexandria :serapeum :fiveam :cl-yesql)
  (:shadowing-import-from :cl-yesql :import)
  (:import-from :trivia :match)
  (:import-from :cl-yesql/statement
    :whitelist
    :parameter
    :parameter-var
    :parameter-whitelist
    :statement)
  ;; Make sure there are no package problems.
  (:import-from :cl-yesql/postmodern)
  (:import-from :cl-yesql/sqlite)
  (:import-from :cl-yesql/sqlite-prepared)
  (:import-from :esrap :parse)
  (:export :run-tests))
(in-package :cl-yesql/test)

(defun run-tests ()
  (run! 'cl-yesql))

(def-suite cl-yesql)
(in-suite cl-yesql)

(def-suite parser :in cl-yesql)
(in-suite parser)

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

(def whitelist-query
  (trim-whitespace
   "
-- name: player-row
SELECT * from player
 WHERE ?{player_name, email, display_name} = ?"))

(def whitelist-query/keywords
  (trim-whitespace
   "
-- name: player-row
SELECT * from player
 WHERE :col{player_name, email, display_name} = :val"))

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
       ((list '&key _)
        t)))))

(test positional-and-keyword-args
  "Mixing positional and keyword args should work."
  (is-true
   (let* ((query (parse-query young-user-names-by-country))
          (args (query-args query)))
     (match args
       ((list _ _ '&key _)
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

(test annotation-beats-affix
  "Check that an annotation beats an affix."
  (is (eql :column
           (query-annotation
            (parse-query
             (trim-whitespace "
-- name: count-xs! @column
select * from xs"))))))

(test whitelist
  (is (equal nil (parse 'whitelist "{}")))
  (is (equal nil (parse 'whitelist "{ }")))
  (is (equal '("x") (parse 'whitelist "{x}")))
  (is (equal '("x") (parse 'whitelist "{ x}")))
  (is (equal '("x") (parse 'whitelist "{x }")))
  (is (equal '("x") (parse 'whitelist "{x,}")))
  (is (equal '("x") (parse 'whitelist "{ x,}")))
  (is (equal '("x") (parse 'whitelist "{ x, }")))
  (is (equal '("x" "y") (parse 'whitelist "{x,y}")))
  (is (equal '("x" "y") (parse 'whitelist "{x,y,}")))
  (is (equal '("x" "y") (parse 'whitelist "{x,y,}")))
  (is (equal '("x" "y") (parse 'whitelist "{x,y,}")))
  (is (equal '("x" "y" "z") (parse 'whitelist "{x,y,z}")))
  (is (equal '("x" "y" "z") (parse 'whitelist "{x, y, z}")))
  (is (equal '("x" "y" "z") (parse 'whitelist "{x , y , z}"))))

(defun parse-whitelist (x)
  (parameter-whitelist (parse 'parameter x)))

(test whitelist-parameter
  (is (null (parse-whitelist "?")))
  (is (null (parse-whitelist ":foo")))
  (is (equal '("x") (parse-whitelist "?{x}")))
  (signals error
    (parse 'parameter "? {x}"))

  (is (equal '("x") (parse-whitelist ":foo{x}")))
  (signals error
    (parse 'parameter ":foo {x}")))

(test whitelist-parameter-not-recursive
  "Whitelist parameters should not be recursive."
  (signals error
    (parse 'parameter "?{x}{y}")))

(test duplicates-in-whitelist
  "Whitelists should not contain duplicates."
  (let ((string "{x,x}"))
    (signals error
      (parse 'whitelist string))
    (is (equal '("x")
               (handler-bind ((error #'continue))
                 (parse 'whitelist string))))))

(test parse-statement-with-whitelist
  (finishes (parse-query whitelist-query)))

(test whitelist-default
  (is-true
   (find '(col "player_name")
         (query-args
          (parse-query whitelist-query/keywords))
         :test #'equal)))

(test build-tree-with-whitelist
  (is
   (equal
    (butlast
     (build-query-tree
      (parse-query whitelist-query)
      (lambda (q)
        (list 'quote (query-vars q)))))
    `(string-case cl-yesql/statement::?0
       ("player_name"
        '(cl-yesql/statement::?1))
       ("email"
        '(cl-yesql/statement::?1))
       ("display_name"
        '(cl-yesql/statement::?1))))))

(test single-line-comment-in-statement
  (let ((string
          (first
           (parse 'statement
                  "CREATE TABLE player_group (
    player_group_name text   NOT NULL,
    -------------
    player_group_id   serial NOT NULL PRIMARY KEY);
"))))
    (is-true (search "player_group_id" string))))

(test trailing-comment
  (finishes
    (parse 'statement "hello -- world"))
  (signals error
    (parse 'statement "hello /* world")))

(test ignore-params-in-comments
  (is (= 3 (length (parse 'statement "select *? * from table"))))
  (is (= 1 (length (parse 'statement "select /* *? */ * from table")))))

(test named-placeholder
  (is (equal
       '(id)
       (query-args
        (parse-query
         "-- name: select-player-by-id @row
-- Returns a player whose ID matches the argument.
SELECT id, login, email, name, pass_hash, pass_salt,
       activatedp, creation_time, last_edit_time FROM player
  WHERE id = ?id LIMIT 1;")))))

(test named-placeholder-lambda
  (is (equal '(player-id player-group-id is-owner)
             (query-args
              (parse-query
               "-- name: add-player-into-player-group @execute
-- Adds a player to a player group.
INSERT INTO players_groups (player_id, player_group_id, is_owner)
  VALUES(?player_id, ?player_group_id, ?is_owner)")))))

(test named-placeholders-with-repeats
  (is (= 3
         (length
          (query-args
           (parse-query
            "-- name: q
INSERT INTO players_groups (player_id, other_player_id, player_group_id, is_owner)
  VALUES(?player_id, ?player_id, ?player_group_id, ?is_owner)"))))))

(def-suite sqlite :in cl-yesql)
(in-suite sqlite)

(yesql:import sqlite-test
  :from "t/test.sql"
  :binding :all-as-functions)

(def db-data
  '(("Thomas Young" 17 "GB")
    ("Yuengling" 17 "US")
    ("Oldsmobile" 50 "US")
    ("Elder Wand" 50 "US")
    ("Alfred Whitehead" 60 "GB"))
  "Test data, lists of name, age, and country code.")

(defun prep-example-db (db)
  (create-user-table db)
  (loop for (name age country-code) in db-data
        do (add-user db
                     :name name
                     :age age
                     :country-code country-code))
  (values))

(test readme-examples
  (sqlite:with-open-database (db ":memory:")
    ;; Prep the DB.
    (prep-example-db db)
    (let ((young-users (young-users-by-country db "US" "GB" :max-age 18)))
      (is (set-equal
           (mapcar #'first young-users)
           (mapcar #'first
                   (filter (op (< (second _) 18))
                           db-data))
           :test #'equal)))
    (is (= 3 (caar (users-by-country db :country-code "US"))))
    (is (= (user-count db) (length db-data)))
    (is (equal (user-names db :order "ASC")
               (map 'list #'first
                    (sort-new db-data #'string<
                              :key #'first))))
    (is (equal (user-names db :order "DESC")
               (map 'list #'first
                    (sort-new db-data #'string>
                              :key #'first))))))
