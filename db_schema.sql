-- The database schema description and the initial data.
-- Execute psql MYDATABASENAME -f PATH_TO_THIS_FILE to apply it.

begin;

create table config (
       key varchar not null primary key,
       value varchar not null
);

-- The schema version is to be incremented on every schema change.
insert into config values ('schema_version', '9');

create table news_versions (
       news_version_id serial not null primary key,
       title varchar not null,
       body varchar not null
);

create table news (
       news_id serial not null primary key,
       news_version_id integer not null references news_versions,
       "date" date not null
);

create table mime_types (
       mime_type_id serial not null primary key,
       value varchar not null unique
);

create table images (
       image_id serial not null primary key,
       content bytea not null,
       mime_type_id integer not null references mime_types
);

create table users (
       user_id serial not null primary key,
       first_name varchar,
       last_name varchar not null,
       avatar_id integer references images,
       created_at timestamp with time zone not null,
       is_admin boolean not null default false,
       token_hash bytea not null
);

create table authors (
       author_id serial not null primary key,
       user_id integer not null references users,
       description varchar not null
);

create table categories (
       category_id serial not null primary key,
       parent_id integer references categories,
       name varchar not null
);

create table tags (
       tag_id serial not null primary key,
       name varchar unique not null
);

commit;
