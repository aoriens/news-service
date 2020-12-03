begin;

update config set value = '22' where key = 'schema_version';

drop view extended_users;

create view extended_users as
select *,
       coalesce(users.first_name || ' ' || users.last_name, users.last_name) as first_to_last_name
from users;

commit;
