CREATE TABLE IF NOT EXISTS semesters
    ( id       INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY
    , title    TEXT    NOT NULL
    );

CREATE INDEX ON semesters(id, title);
