begin;

update config set value = '16' where key = 'schema_version';

create view extended_users as
select *,
       coalesce(users.first_name || ' ' || users.last_name, users.last_name) as first_to_last_name
from users;

commit;
