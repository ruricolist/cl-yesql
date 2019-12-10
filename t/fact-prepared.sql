#lang cl-yesql/sqlite-prepared

-- name: add-fact @execute
INSERT INTO fact (subject, fact) VALUES (?, ?)
