CL-Yesql is a Common Lisp library for *using* SQL, based on Clojure’s
[Yesql][].

Besides being useful in itself, this library also serves as a
demonstration of writing a language for [Vernacular][], as well as the
advantages of allowing (as Vernacular does) the same file to be loaded
as different modules in different languages.

# The Yesql file

For SQL files, the syntax supported by cl-yesql is (or should be) a
superset of the syntax supported by
[the original Clojure library][syntax].

## Extensions to Yesql

### Annotations

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

### Whitelists

Sometimes you want to define a query that is a trivial variation of another query.

``` sql
-- name: user-names-asc
SELECT name FROM user_name ORDER BY name ASC

-- name: user-names-desc
SELECT name FROM user_name ORDER BY name DESC
```

Obviously it would be better if you could define these as one query
with a parameter. No SQL database, however, lets you pass ASC and DESC
as parameters. The same goes for column names, table names, etc.

Our solution to this problem is to allow specifying a parameter with a
*whitelist*. The whitelist consists of a set of comma-separated
options appearing directly after a parameter.

``` sql
-- name: user-names
SELECT name FROM user_name ORDER BY name :order{ASC,DESC}
```

This gives you the flexibility to define multiple queries in a single
function without opening the door to SQL injection.

# Importing

Importing from Yesql files is done in the usual way, through Vernacular.

At the moment, that looks like this:

    ;; Importing everything.
    (yesql:import my-queries
      :from "sql/queries.sql"
      :as :cl-yesql/postmodern
      :binding :all-as-functions)

    ;; Importing individual functions.
    (yesql:import my-queries
      :from "sql/queries.sql"
      :as :cl-yesql/postmodern
      :binding (#'database-size #'thing-tags))

Note that paths are relative to the base of the system, not the
current file.

Note also that the `:cl-yesql/postmodern` system must be loaded before
the above code is compiled.

## Languages

# The Postgres language

Support for Postgres is provided by the package
`:cl-yesql/postmodern`, provided by the system of the same name.
Obviously it builds on the [Postmodern][] library.

The Postmodern language implicitly prepares (using
`postmodern:prepare`) all queries when the Yesql file is loaded. It is
not necessary to do anything else to prepare them.

## The SQLite languages

Support for SQLite is provided through two languages, with different
semantics.

### Simple SQLite

For querying from an SQLite database, and for discrete inserts and
updates, the right language to use is `:cl-yesql/sqlite`, provided by
the system of the same name.

Note that (unlike for the Postmodern language) functions exported by
the SQLite language expect a database handle as their first argument.

### Prepared SQLite

The language `:cl-yesql/sqlite-prepared` is designed for bulk inserts.
The functions exported by the prepared SQLite language are not
intended for direct use. Instead, they return templates for use with
`cl-yesql/sqlite-prepared:with-prepared-statement`.

    (yesql:import sqlite-prepared
      :from "sql/sqlite.sql"
      :as :cl-yesql/sqlite-prepared
      :values (#'record-kv))

    (defun save-kv-data (db plist)
      (sqlite:with-transaction db
        (cl-yesql/sqlite-prepared:with-prepared-statement
            (record #'record-kv db)
          (doplist (k v data)
            (record :key (string k) :value v)))))

The language `:cl-yesql/sqlite-prepared` is provided by the package of
the same name.

## Other languages?

If you want to add another SQL backend to Yesql, I suggest you begin
by looking over one of the existing implementations.
The [SQLite integration](sqlite.lisp) is the simplest. Essentially all
you need to do is to define a package to serve as the language and, in
that package, create an appropriate binding for `defquery`.

[Yesql]: https://github.com/krisajenkins/yesql
[syntax]: https://github.com/krisajenkins/yesql#one-file-many-queries
[Overlord]: https://github.com/ruricolist/overlord
[Vernacular]: https://github.com/ruricolist/vernacular
[Postmodern]: http://marijnhaverbeke.nl/postmodern/
