begin;

update config set value = '8' where key = 'schema_version';

alter table tags add unique (name);

commit;
