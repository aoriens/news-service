begin;

update config set value = '21' where key = 'schema_version';

alter table news_versions
      drop constraint news_versions_author_id_fkey,
      add foreign key (author_id) references authors on delete set null;

commit;
