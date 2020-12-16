begin;

update config set value = '24' where key = 'schema_version';

alter table news_versions
      add created_from_news_id integer references news(news_id);

drop view drafts;

create view drafts as
       select news_versions.*
       from news_versions left join news using (news_version_id)
       where news_id is null;

commit;
