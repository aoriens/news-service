create table config (
       key varchar not null primary key,
       value varchar not null
);

-- The schema version is to be incremented on every schema change.
insert into config values ('schema_version', '1');

create table news (
       id serial not null primary key,
       title varchar not null,
       body varchar not null,
       "date" date not null
);
