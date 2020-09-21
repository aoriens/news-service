begin;

update config set value = '6' where key = 'schema_version';

create table categories (
       category_id serial not null primary key,
       parent_id integer references categories,
       name varchar not null
);

commit;
