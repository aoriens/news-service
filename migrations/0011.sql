begin;

update config set value = '11' where key = 'schema_version';

create table news_versions_and_tags_relation (
       news_version_id integer not null references news_versions,
       tag_id integer not null references tags,
       primary key (news_version_id, tag_id)
);

create table news_versions_and_additional_photos_relation (
       news_version_id integer not null references news_versions,
       image_id integer not null references images,
       primary key (news_version_id, image_id)
);

commit;
