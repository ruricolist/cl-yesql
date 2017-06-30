CL-Yesql is a Common Lisp library for *using* SQL, based on Clojure’s
[Yesql][].

Besides being useful in itself, this library also serves as a
demonstration of writing a language for [Overlord][], as well as the
advantages of allowing (as Overlord does) the same file to be loaded
as different modules in different languages.

# The Yesql file

For SQL files, the syntax supported by cl-yesql is (or should be) a
superset of the syntax supported by
[the original Clojure library][syntax].

CL-Yesql understands more affixes than Clojure’s Yesql does. The
original understands `fn!` (meaning that the function returns nothing)
and `fn<!` (meaning that the function should return the last id).
CL-Yesql also understands `fn?` or `fn-p` (meaning that the function
returns a boolean) and `count-fn` (meaning that the function returns a
number).

However, affixes are a limited solution. CL-Yesql lets you annotate a
function definition with a specification of what the function returns.

    -- name: users-by-country @rows
    -- Counts the users in a given country.
    SELECT count(*) AS count
    FROM user
    WHERE country_code = :country_code

    -- name: user-count @single
    -- Counts all the users.
    SELECT count(*) AS count
    FROM user

    -- name: young-user-names-by-country @column
    SELECT name
    FROM user
    WHERE (
      country_code = ?
      OR
      country_code = ?
    )
    AND age < :max_age

The full list of annotations:

Annotation | Meaning
---------- | -------
@rows      | default
@row       | one row
@values    | one row as multiple values
@column    | one column
@single    | a single value
@execute   | no return value (same as !)
@last-id   | ID of the last row inserted (same as <!)

While the exact signature of functions depends on the language
(Postmodern does not require passing in the database connection, for
example, while SQLite does), you may expect to provide positional
arguments and keyword arguments as you would to any Lisp function.

    (users-by-country :country-code "USA")

    (young-user-names-by-country "GB" "US" :max-age 18)

# Importing

Importing from Yesql files is done in the usual way, through Overlord.

At the moment, that looks like this:

    ;; Importing everything.
    (overlord:import my-queries
      :from "sql/queries.sql"
      :as :cl-yesql/postmodern
      :binding :all-as-functions)

    ;; Importing individual functions.
    (overlord:import my-queries
      :from "sql/queries.sql"
      :as :cl-yesql/postmodern
      :binding (#'database-size #'thing-tags))

Note that paths are relative to the base of the system, not the
current file.

Overlord is still experimental, however, so the syntax may change.

## Languages

# The Postgres language

Support for Postgres is provided by the package
`:cl-yesql/postmodern`. Obviously it builds on the [Postmodern][]
library.

The Postmodern language implicitly prepares (using
`postmodern:prepare`) all queries when the Yesql file is loaded. It is
not necessary to do anything else to prepare them.

## The SQLite languages

Support for SQLite is provided through two languages, with different
semantics.

### Simple SQLite

For querying from an SQLite database, and for discrete inserts and
updates, the right language to use is `:cl-yesql/sqlite`.

Note that (unlike for the Postmodern language) functions exported by
the SQLite language expect a database handle as their first argument.

### Prepared SQLite

The language `:cl-yesql/sqlite-prepared` is designed for bulk inserts.
The functions exported by the prepared SQLite language are not
intended for direct use. Instead, they return templates for use with
`cl-yesql/sqlite-prepared:with-prepared-statement`.

    (overlord:import sqlite-prepared
      :from "sql/sqlite.sql"
      :as :cl-yesql/sqlite-prepared
      :values (#'record-kv))

    (defun save-kv-data (db plist)
      (sqlite:with-transaction db
        (cl-yesql/sqlite-prepared:with-prepared-statement
            (record #'record-kv db)
          (doplist (k v data)
            (record :key (string k) :value v)))))

## Other languages?

If you want to add another SQL backend to Yesql, I suggest you begin
by looking over one of the existing implementations.
The [SQLite integration](sqlite.lisp) is the simplest. Essentially all
you need to do is to define a package to serve as the language and, in
that package, create an appropriate binding for `defquery`.

[Yesql]: https://github.com/krisajenkins/yesql
[syntax]: https://github.com/krisajenkins/yesql#one-file-many-queries
[Overlord]: https://github.com/TBRSS/overlord
[Postmodern]: http://marijnhaverbeke.nl/postmodern/
