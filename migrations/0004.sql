begin;

update config set value = '4' where key = 'schema_version';

alter table news       rename id to news_id;
alter table mime_types rename id to mime_type_id;
alter table images     rename id to image_id;
alter table users      rename id to user_id;

commit;
