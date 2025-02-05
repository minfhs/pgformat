# Opinionated PostgreSQL formatter

This is _our_ style. Feel free to try it out, but expect not to like it.

## Install

```sh
just up
just install # installs to /usr/local/bin
```

## Play

```sh
# Run tests in watchmode
dune runtest -w
# Edit source and see if tests still pass
```

Manually run a test on a specific file

```sh
dune exec pgformat -- ./fixture/complex.sql
```

### Helix

Edit your languages toml.

```toml
[[language]]
name = "sql"
auto-format = true
formatter = { command = "pgformat" }
```

Check `hx --health sql`.

### Visual Studio Code

You can install an extension following this [guide](./.vscode/README.md).

## Sample format

You can also check [fixture](./fixture) as they are all formatted with the latest version.

### Before

```sql
SELECT
    reviews.id AS id , applicants.fhs_id AS fhs_id , applicants.id AS applicant_id
    , reviews.room_id , reviews.user_id , reviews.state AS "state: ReviewState"
    , applicants.state AS "applicant_state: ApplicantState"
FROM applicant_reviews AS reviews LEFT JOIN (
    SELECT id , fhs_id , state FROM applicants ) AS applicants ON applicants.id
    = reviews.applicant_id
WHERE
    reviews.room_id = ( SELECT id FROM rooms
        WHERE abbr = $1 AND y = x )
    AND applicants.state IS NOT NULL AND applicants.fhs_id IS NOT NULL;
```

### After

```sql
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
```
