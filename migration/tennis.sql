insert into public.user(id,password, username, created_at, updated_at) select id,password,username,date_joined, date_joined from auth_user;

--insert into public.user(id,password,last_login, is_superuser, username, first_name, last_name, email, is_staff, is_active, date_joined) select id,password,last_login, is_superuser, username, first_name, last_name, email, is_staff, is_active, date_joined from auth_user;
-- alter table auth_user rename to auth_user_bak;
-- alter table user rename to auth_user;

Select nextval(pg_get_serial_sequence('user', 'id')) as new_id;
SELECT setval(pg_get_serial_sequence('user', 'id'), (select max(id) from public.user), true);