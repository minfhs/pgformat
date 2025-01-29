open! Core
open Helper

let%expect_test "Simple SELECT" =
  format_script "SELECT a,b FROM x as y;";
  [%expect {|
    SELECT
        a
        , b
    FROM x AS y;
    |}]
;;

let%expect_test "Complex SELECT" =
  format_script {| SELECT
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
    AND applicants.fhs_id IS NOT NULL; |};
  [%expect {|
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
