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
