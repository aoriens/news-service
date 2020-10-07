begin;

update config set value = '7' where key = 'schema_version';

create table tags (
       tag_id serial not null primary key,
       name varchar not null
);

commit;
