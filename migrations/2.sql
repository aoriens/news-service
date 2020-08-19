update config set value = '2' where key = 'schema_version';

create table mime_types (
       id serial not null primary key,
       value varchar not null unique
);

create table images (
       id serial not null primary key,
       content bytea not null,
       mime_type_id integer not null references mime_types
);

create table users (
       id serial not null primary key,
       first_name varchar,
       last_name varchar not null,
       avatar_id integer references images,
       created_at timestamp with time zone not null,
       is_admin boolean not null default false,
       token_hash bytea not null,
       token_hash_algorithm integer not null
);
