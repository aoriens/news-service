begin;

update config set value = '18' where key = 'schema_version';

alter table news_versions_and_tags_relation
      drop constraint news_versions_and_tags_relation_news_version_id_fkey,
      drop constraint news_versions_and_tags_relation_tag_id_fkey;

alter table news_versions_and_tags_relation
      add foreign key (news_version_id)  references news_versions on delete cascade,
      add foreign key (tag_id) references tags on delete cascade;

commit;
