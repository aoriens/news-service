begin;

update config set value = '5' where key = 'schema_version';

create table authors (
       author_id serial not null primary key,
       user_id integer not null references users,
       description varchar not null
);

commit;
