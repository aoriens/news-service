begin;

update config set value = '23' where key = 'schema_version';

alter table news_versions
      alter category_id drop not null;

commit;
