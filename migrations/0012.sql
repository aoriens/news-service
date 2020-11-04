begin;

update config set value = '12' where key = 'schema_version';

alter table news add unique (news_version_id);

commit;
