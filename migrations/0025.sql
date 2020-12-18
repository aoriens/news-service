begin;

update config set value = '25' where key = 'schema_version';

drop view drafts;

create table drafts (
       draft_id serial not null primary key,
       news_version_id integer not null unique references news_versions,
       created_from_news_id integer references news(news_id)
);

-- news_version_id was also used as an identifier of a draft, so ids
-- of drafts created so far must be kept equal to ids of the
-- corresponding news versions. They may diverge later.
insert into drafts(draft_id, news_version_id, created_from_news_id)
select news_version_id, news_version_id, created_from_news_id
from news_versions
where news_version_id not in (select news_version_id from news);

select setval('drafts_draft_id_seq', nextval('news_versions_news_version_id_seq' :: regclass), false);

alter table news_versions
      drop created_from_news_id;

commit;
