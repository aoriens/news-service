begin;

update config set value = '19' where key = 'schema_version';

alter table users
      add is_deleted boolean not null default false;

commit;
