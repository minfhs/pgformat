open! Core
open Helper

let%expect_test "Keep comments" =
  format_script
    {|/*
This
is a
comment
*/
SELECT a,b FROM x as y;|};
  [%expect
    {|
    /*
    This
    is a
    comment
    */
    SELECT
        a
        , b
    FROM x AS y;
    |}]
;;

let%expect_test "Simple SELECT" =
  format_script "SELECT a,b FROM x as y;";
  [%expect
    {|
    SELECT
        a
        , b
    FROM x AS y;
    |}]
;;

let%expect_test "Simple CREATE" =
  format_script
    {|
    CREATE TABLE IF NOT EXISTS semesters
    ( id       INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY
    , title    TEXT    NOT NULL
    );

    CREATE INDEX ON semesters(id, title);|};
  [%expect
    {|
    CREATE TABLE IF NOT EXISTS semesters (
        id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY
        , title TEXT NOT NULL
    );

    CREATE INDEX ON semesters (
        id
        , title
    );
    |}]
;;

let%expect_test "Simple SELECT" =
  format_script
    {|
    INSERT INTO users (first_name, last_name, email, kind)
    VALUES
        ('John', 'CG', 'john@cg.com', 'student'),
        ('Jane', 'CG', 'jane@cg.com', 'student'),
        ('Bob', 'CG', 'bob@cg.com', 'student'),
        ('John', 'RM', 'john@rm.com', 'student'),
        ('Jane', 'RM', 'jane@rm.com', 'student'),
        ('Bob', 'RM', 'bob@rm.com', 'student'),
        ('John', 'TS', 'john@ts.com', 'student'),
        ('Jane', 'TS', 'jane@ts.com', 'student'),
        ('Bob', 'TS', 'bob@ts.com', 'student')
        ;
  |};
  [%expect
    {|
    INSERT INTO users (
        first_name
        , last_name
        , email
        , kind
    ) VALUES ('John', 'CG', 'john@cg.com', 'student')
        , ('Jane', 'CG', 'jane@cg.com', 'student')
        , ('Bob', 'CG', 'bob@cg.com', 'student')
        , ('John', 'RM', 'john@rm.com', 'student')
        , ('Jane', 'RM', 'jane@rm.com', 'student')
        , ('Bob', 'RM', 'bob@rm.com', 'student')
        , ('John', 'TS', 'john@ts.com', 'student')
        , ('Jane', 'TS', 'jane@ts.com', 'student')
        , ('Bob', 'TS', 'bob@ts.com', 'student');
    |}]
;;

let%expect_test "Advanced Funct Seed" =
  format_script
    {|
    CREATE FUNCTION seed_optional_for(
        user_email TEXT
    )
    RETURNS void AS $$
    DECLARE
        u_id integer;
        r_id integer;
        s record;
    BEGIN
        u_id := (
            SELECT
                id
            FROM users
            WHERE email = user_email);
        FOR s IN
            SELECT
                id
            FROM schedules
            WHERE
                semester_id = 1
                AND kind = 'optin'
        LOOP
            r_id := (
                SELECT
                    id
                FROM rooms
                WHERE
                    abbr = (
                        SELECT (
                            array['YG', '3D'])[mod(random()::integer,2)::integer + 1] ));
            INSERT INTO schedule_enrollments (
                schedule_id
                , user_id
                , room_id)
            VALUES (
                s.id
                , u_id
                , r_id); 

            INSERT INTO  user_enrollments (
                user_id
                , room_id)
            VALUES (u_id,r_id);
        END LOOP;
    END;
    $$ LANGUAGE plpgsql;
    |};
  [%expect
    {|
    CREATE FUNCTION seed_optional_for (
        user_email TEXT
    )
    RETURNS void AS
    $$
    DECLARE
        u_id integer;
        r_id integer;
        s record;

    BEGIN
        u_id := (
            SELECT
                id
            FROM users
            WHERE
                email = user_email
        );
        FOR s IN
            SELECT
                id
            FROM schedules
            WHERE
                semester_id = 1
                AND kind = 'optin'
        LOOP
            r_id := (
                SELECT
                    id
                FROM rooms
                WHERE
                    abbr = (
                        SELECT (
                            array ['YG', '3D']
                        ) [mod(random()::integer,2)::integer + 1]
                    )
            );

            INSERT INTO schedule_enrollments (
                schedule_id
                , user_id
                , room_id
            ) VALUES (s.id, u_id, r_id);

            INSERT INTO user_enrollments (
                user_id
                , room_id
            ) VALUES (u_id, r_id);
        END LOOP;
    END;
    $$ LANGUAGE plpgsql;
    |}]
;;

let%expect_test "Complex SELECT" =
  format_script
    {| SELECT
    reviews.id AS id
    , applicants.fhs_id AS fhs_id
    , applicants.id AS applicant_id
    , reviews.room_id
    , reviews.user_id
    , reviews.state AS "state: ReviewState"
    , applicants.state AS "applicant_state: ApplicantState"
FROM applicant_reviews AS reviews
LEFT JOIN ( SELECT id , fhs_id , state FROM applicants ) AS applicants ON applicants.id = reviews.applicant_id
WHERE
    reviews.room_id = ( SELECT id FROM rooms WHERE abbr = $1 AND y = x )
    AND applicants.state IS NOT NULL
    AND applicants.fhs_id IS NOT NULL; |};
  [%expect
    {|
    SELECT
        reviews.id AS id
        , applicants.fhs_id AS fhs_id
        , applicants.id AS applicant_id
        , reviews.room_id
        , reviews.user_id
        , reviews.state AS "state: ReviewState"
        , applicants.state AS "applicant_state: ApplicantState"
    FROM applicant_reviews AS reviews
    LEFT JOIN (
        SELECT
            id
            , fhs_id
            , state
        FROM applicants
    ) AS applicants ON applicants.id = reviews.applicant_id
    WHERE
        reviews.room_id = (
            SELECT
                id
            FROM rooms
            WHERE
                abbr = $1
                AND y = x
        )
        AND applicants.state IS NOT NULL
        AND applicants.fhs_id IS NOT NULL;
    |}]
;;

let%expect_test "Keep comments" =
  format_script {|INSERT INTO yo (a,b) VALUES (1, 'OK', '"FAIL"', '{"a":1.2}');|};
  [%expect
    {|
    INSERT INTO yo (
        a
        , b
    ) VALUES (1 , 'OK', '"FAIL"', '{"a":1.2}');
    |}]
;;
