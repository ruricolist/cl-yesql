CL-Yesql is a Common Lisp library for *using* SQL, based on Clojure’s
[Yesql][]. SQL statements live in their own files, in SQL syntax, and
are imported into Lisp as functions. You are not limited to the
features a DSL supports, and you can make use of SQL support in your
preferred editor, or any other tools that works with SQL syntax.

CL-Yesql is built on top of [Vernacular][], a framework for embedding
languages in Common Lisp. Different backends (Postgres, SQLite) are
provided as different Vernacular languages. Before you can use CL-Yesql, you will need to load the system that provides the particular language you need. E.g. `cl-yesql/postmodern` for Postgres, or `cl-yesql/sqlite` SQLite.

Besides being useful in itself, this library also serves as a
demonstration of writing languages for Vernacular.

# Syntax

## SQL files

SQL statements are defined in separate files.

Each SQL statements is defined in at least two lines:

1. The name of the function.
2. Optionally, a docstring.
3. One or more lines representing the SQL statement itself.

Statements are separated by spaces.

``` sql
-- name: uptime
-- Get the database uptime.
SELECT pg_postmaster_start_time()

-- name: live-dead-tuples
-- Return the number of live and dead tuples in each table.
SELECT relname, n_live_tup, n_dead_tup FROM pg_stat_user_tables

```

# Importing

Importing from Yesql files is done in the usual way for Vernacular. That looks like this:

    ;; Import everything from a file.
    (yesql:import my-queries
      :from "sql/queries.sql"
      :as :cl-yesql/postmodern
      :binding :all-functions)

    ;; Importing individual functions.
    (yesql:import my-queries
      :from "sql/queries.sql"
      :as :cl-yesql/postmodern
      :binding (#'uptime #'live-dead-tuples))

To find your files, CL-Yesql looks for a system with the same name as the current package. If it cannot find your files, you may need to use `overlord:set-package-base` to tell it where to look.

Note that paths are relative to the base of the system, not the
current file.

Note also that the `:cl-yesql/postmodern` system must be loaded before
the above code is compiled.

## Parameters

Parameters can be positional parameters or keyword parameters. You can use both positional and keyword parameters in the same statement.

Keyword parameters start with a colon.

``` sql
-- name: users-by-country
-- Counts the users in a given country.
SELECT count(*) AS count
FROM user
WHERE country_code = :country_code

```

Positional parameters use `?` as a placeholder.

``` sql
-- name: young-users-by-country
SELECT *
FROM user
WHERE (
  country_code = ?
  OR
  country_code = ?
)
AND age < :max_age
```

While the exact signature of the generated Lisp functions depends on
the language (Postmodern does not require passing in the database
connection, for example, while SQLite does), you provide positional
arguments and keyword arguments as you would to any Lisp function.

    (users-by-country :country-code "USA")

    (young-user-names-by-country "GB" "US" :max-age 18)


## Affixes

For simple cases, CL-Yesql looks at the function name to understand what it does, based on its affixes: `fn!` returns nothing, `fn<!` returns the last ID, `fn?` returns a boolean, and `count-fn` returns a number.

``` sql
-- name: hide-article!
-- Returns nothing.
INSERT INTO hidden_article (user_id, article_id) VALUES (?, ?)
    ON CONFLICT DO NOTHING

-- name: create-person<!
-- Returns the ID of the new record.
INSERT INTO person (name) VALUES (:name)

-- name: article-hidden?
-- Returns a single value.
SELECT EXISTS (
    SELECT 1 FROM hidden_article
    WHERE user_id = ?
        AND article_id = ?)

-- name: count-items-stored
-- Returns a single value.
SELECT reltuples FROM pg_class WHERE relname = 'item'
```

### Annotations

Affixes, while useful, are a limited solution. CL-Yesql also lets you
annotate a function definition with exactly what the function returns.

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
@rows      | default, a list of lists
@row       | one row, a list
@values    | one row as multiple values
@column    | one column, a list
@single    | a single value
@execute   | no return value (same as !)
@last-id   | ID of the last row inserted (same as <!)
@setter    | A setf function.

#### Setters

Yesql can be used to define setf functions using the `@setter` annotation:

    -- name: user-age @single
    SELECT age FROM user WHERE name = ?

    -- name: user-age @setter
    UPDATE user SET age = ? where name = ?

When you use `:all-functions`, setters sharing the same name as a function (like `user-age` above) are imported along with their namesakes.

Setters can be explicitly imported using a slightly different syntax:

    (yesql:import user-queries
      :from "sql/users.sql"
      :as :cl-yesql/sqlite
      :binding (#'user-age #’(setf user-age))

    (yesql:import user-queries
      :from "sql/users.sql"
      :binding :all-setters)

Using `:all-setters` imports all the setters, and only the setters. This can be useful if you define setters without equivalent readers. It can still be combined with `:all-functions`:

    (yesql:import user-queries
      :from "sql/users.sql"
      :binding (:all-functions :all-setters))

A setter must have at least one positional argument. When using
setters, be careful to make sure that the positional arguments are in
the right order, so the value argument comes first. (This is the
natural order when using `UPDATE`.)

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

The language `:cl-yesql/sqlite-prepared` is designed for efficient
bulk inserts. The functions exported by the prepared SQLite language
are not intended for direct use. Instead, they return templates for
use with `cl-yesql/sqlite-prepared:with-prepared-statement`.

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

The language `:cl-yesql/sqlite-prepared` is provided by the system of
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
