begin;

update config set value = '15' where key = 'schema_version';

create function news_version_ids_connected_with_all_tags_with_ids(
    required_tag_ids integer array
    ) returns table(news_version_id integer)
language sql
as $$
    select news_version_id
    from news_versions
    where not exists (
        select 1
        from unnest(required_tag_ids) as r(required_id)
        where required_id not in (
            select tag_id
            from news_versions_and_tags_relation as rel
            where rel.news_version_id = news_versions.news_version_id
        )
    )
$$;

create function news_version_ids_connected_with_all_tags_like(
    required_patterns varchar array
    ) returns table(news_version_id integer)
language sql
as $$
    select news_version_id
    from news_versions
    where not exists (
        select 1
        from unnest(required_patterns) as r(required_pattern)
        where not exists (
            select 1
            from news_versions_and_tags_relation as rel
                 join tags using (tag_id)
            where rel.news_version_id = news_versions.news_version_id
                  and tags.name ilike required_pattern
        )
    )
$$;

commit;
