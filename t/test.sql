#lang cl-yesql/sqlite

-- name: create-user-table
-- Create the table to store the data.
CREATE TABLE user (name text, country_code char(2), age integer)

-- name: add-user
INSERT INTO user (name, country_code, age) VALUES (:name, :country_code, :age)

-- name: young-users-by-country
SELECT *
FROM user
WHERE (
        country_code = ?
        OR
        country_code = ?
        )
    AND age < :max_age

-- name: users-by-country
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

-- name: user-names @column
SELECT name FROM user ORDER BY name :order{ASC,DESC}
