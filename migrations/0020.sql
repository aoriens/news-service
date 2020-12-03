begin;

update config set value = '20' where key = 'schema_version';

alter table news_versions
      alter author_id drop not null;

commit;
