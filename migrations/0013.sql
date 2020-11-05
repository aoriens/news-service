begin;

update config set value = '13' where key = 'schema_version';

create view drafts as
       select news_versions.*
       from news_versions left join news using (news_version_id)
       where news_id is null;

commit;
