INSERT INTO schedules (semester_id, day, start_at, end_at, kind, title)
VALUES
    (1, 'mon', '07:45', '08:00', 'common', 'Frokost'),
    (1, 'mon', '08:30', '08:50', 'common', 'Morgensamling'),
    (1, 'mon', '09:00', '11:00', 'main', null),
    (1, 'mon', '11:15', '11:50', 'common', 'Lunsj'),
    (1, 'mon', '12:00', '14:30', 'main', null),
    (1, 'mon', '15:00', '16:00', 'common', 'Middag'),
    (1, 'mon', '19:00', '20:00', 'common', 'Kveldsmat'),
    (1, 'mon', '23:00', '23:00', 'common', 'Ro'),
    (1, 'tue', '07:45', '08:00', 'common', 'Frokost'),
    (1, 'tue', '08:30', '08:50', 'common', 'Morgensamling'),
    (1, 'tue', '09:00', '11:00', 'optin', null),
    (1, 'tue', '11:15', '11:50', 'common', 'Lunsj'),
    (1, 'tue', '12:00', '14:30', 'optin', null),
    (1, 'tue', '15:00', '16:00', 'common', 'Middag'),
    (1, 'tue', '17:30', '18:30', 'optin', null),
    (1, 'tue', '19:00', '20:00', 'common', 'Kveldsmat'),
    (1, 'tue', '23:00', '23:00', 'common', 'Ro'),
    (1, 'wed', '07:45', '08:00', 'common', 'Frokost'),
    (1, 'wed', '08:30', '08:50', 'common', 'Morgensamling'),
    (1, 'wed', '09:00', '11:00', 'main', null),
    (1, 'wed', '11:15', '11:50', 'common', 'Lunsj'),
    (1, 'wed', '12:00', '14:30', 'main', null),
    (1, 'wed', '15:00', '16:00', 'common', 'Middag'),
    (1, 'wed', '19:00', '20:00', 'common', 'Kveldsmat'),
    (1, 'wed', '23:00', '23:00', 'common', 'Ro'),
    (1, 'thu', '07:45', '08:00', 'common', 'Frokost'),
    (1, 'thu', '08:30', '08:50', 'common', 'Morgensamling'),
    (1, 'thu', '09:00', '10:00', 'optin', null),
    (1, 'thu', '10:00', '11:00', 'optin', null),
    (1, 'thu', '11:15', '11:50', 'common', 'Lunsj'),
    (1, 'thu', '12:00', '13:00', 'optin', null),
    (1, 'thu', '13:00', '14:00', 'common', 'Allmøte'),
    (1, 'thu', '15:00', '16:00', 'common', 'Middag'),
    (1, 'thu', '19:00', '20:00', 'common', 'Kveldsmat'),
    (1, 'thu', '23:00', '23:00', 'common', 'Ro'),
    (1, 'fri', '07:45', '08:00', 'common', 'Frokost'),
    (1, 'fri', '08:30', '08:50', 'common', 'Morgensamling'),
    (1, 'fri', '09:00', '11:00', 'main', null),
    (1, 'fri', '11:15', '11:50', 'common', 'Lunsj'),
    (1, 'fri', '12:00', '14:30', 'main', null),
    (1, 'fri', '15:00', '16:00', 'common', 'Middag'),
    (1, 'fri', '19:00', '20:00', 'common', 'Kveldsmat'),
    (1, 'fri', '23:00', '23:00', 'common', 'Ro'),
    (1, 'sat', '07:45', '08:00', 'common', 'Frokost'),
    (1, 'sat', '08:30', '08:50', 'common', 'Morgensamling'),
    (1, 'sat', '09:00', '11:00', 'common', 'Seminar'),
    (1, 'sat', '11:15', '11:50', 'common', 'Lunsj'),
    (1, 'sat', '15:00', '16:00', 'common', 'Middag'),
    (1, 'sat', '19:00', '20:00', 'common', 'Kveldsmat'),
    (1, 'sat', '23:00', '23:00', 'common', 'Ro'),
    (1, 'sun', '08:30', '09:30', 'common', 'Frokost'),
    (1, 'sun', '12:00', '13:00', 'common', 'Lunsj'),
    (1, 'sun', '16:00', '17:00', 'common', 'Middag'),
    (1, 'sun', '20:00', '21:00', 'common', 'Kveldsmat'),
    (1, 'sun', '23:00', '23:00', 'common', 'Ro')
    ;

CREATE FUNCTION seed_optional_for(user_email TEXT)
RETURNS void AS $$
DECLARE
    u_id integer;
    r_id integer;
    s record;
BEGIN
    u_id := (SELECT id FROM users WHERE email = user_email);
    FOR s IN SELECT id FROM schedules
        WHERE semester_id = 1 AND kind = 'optin'
    LOOP
        r_id :=(SELECT id FROM rooms WHERE abbr = (select (array['YG', '3D'])[mod(random()::integer,2)::integer + 1]));
        INSERT INTO schedule_enrollments
            (schedule_id, user_id, room_id)
        VALUES (s.id, u_id, r_id); 

        INSERT INTO  user_enrollments (user_id, room_id)
        VALUES (u_id,r_id);
    END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION seed_schedule_for(room_abbr TEXT, user_email TEXT)
RETURNS void AS $$
DECLARE
    u_id integer;
    r_id integer;
BEGIN
    u_id := (SELECT id FROM users WHERE email = user_email);
    r_id := (SELECT id FROM rooms WHERE abbr = room_abbr);
    INSERT INTO  user_enrollments (user_id, room_id)
    VALUES (u_id,r_id);

    INSERT INTO schedule_enrollments
        (schedule_id, user_id, room_id)
    SELECT
        id, u_id, r_id
    FROM schedules
    WHERE semester_id = 1 AND kind = 'main';
    PERFORM seed_optional_for(user_email);
END;
$$ LANGUAGE plpgsql;

SELECT seed_schedule_for('CG', 'john@cg.com');
SELECT seed_schedule_for('CG', 'jane@cg.com');
SELECT seed_schedule_for('CG', 'bob@cg.com');
SELECT seed_schedule_for('RM', 'john@rm.com');
SELECT seed_schedule_for('RM', 'jane@rm.com');
SELECT seed_schedule_for('RM', 'bob@rm.com');
SELECT seed_schedule_for('TS', 'john@ts.com');
SELECT seed_schedule_for('TS', 'jane@ts.com');
SELECT seed_schedule_for('TS', 'bob@ts.com');
