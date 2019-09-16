#lang cl-yesql/sqlite

-- name: create-facts-table @execute
CREATE TABLE fact (subject text, fact text)

-- name: add-fact @execute
INSERT INTO fact (subject, fact) VALUES (?, ?)

-- name: facts-about @column
SELECT fact FROM fact WHERE subject = ?
