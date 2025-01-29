SELECT
  reviews.id as id,
  applicants.fhs_id as fhs_id,
  applicants.id as applicant_id,
  reviews.room_id,
  reviews.user_id,
  reviews.state as "state: ReviewState",
  applicants.state as "applicant_state: ApplicantState"
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
