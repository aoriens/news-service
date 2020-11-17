begin;

update config set value = '17' where key = 'schema_version';

create table comments (
       comment_id serial not null primary key,
       news_id integer not null references news,
       user_id integer references users,
       text varchar not null,
       created_at timestamp with time zone not null
);

commit;
