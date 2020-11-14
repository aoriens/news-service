-- The database schema description and the initial data.
-- Execute psql MYDATABASENAME -f PATH_TO_THIS_FILE to apply it.

begin;

create table config (
       key varchar not null primary key,
       value varchar not null
);

-- The schema version is to be incremented on every schema change.
insert into config values ('schema_version', '9');

create table mime_types (
       mime_type_id serial not null primary key,
       value varchar not null unique
);

create table images (
       image_id serial not null primary key,
       content bytea not null,
       mime_type_id integer not null references mime_types
);

create table users (
       user_id serial not null primary key,
       first_name varchar,
       last_name varchar not null,
       avatar_id integer references images,
       created_at timestamp with time zone not null,
       is_admin boolean not null default false,
       token_hash bytea not null
);

create table authors (
       author_id serial not null primary key,
       user_id integer not null references users,
       description varchar not null
);

create table categories (
       category_id serial not null primary key,
       parent_id integer references categories,
       name varchar not null
);

create function descendants_of_categories_with_ids(
    ids integer array
    ) returns table(category_id integer)
language sql
$$
    with recursive cats as (
      select category_id
      from categories
      where category_id = any(ids)

      union

      select categories.category_id
      from categories
           join cats on categories.parent_id = cats.category_id
    )
    select *
    from cats
$$;

create function descendants_of_categories_named_like(
    patterns varchar array
    ) returns table(category_id integer)
language sql
as $$
    with recursive cats as (
      select category_id
      from categories
      where name ilike any(patterns)

      union

      select categories.category_id
      from categories
           join cats on categories.parent_id = cats.category_id
    )
    select *
    from cats
$$;

create table tags (
       tag_id serial not null primary key,
       name varchar unique not null
);

create table news_versions (
       news_version_id serial not null primary key,
       title varchar not null,
       body varchar not null,
       author_id integer not null references authors,
       category_id integer not null references categories,
       main_photo_id integer references images
);

create table news_versions_and_tags_relation (
       news_version_id integer not null references news_versions,
       tag_id integer not null references tags,
       primary key (news_version_id, tag_id)
);

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

create table news_versions_and_additional_photos_relation (
       news_version_id integer not null references news_versions,
       image_id integer not null references images,
       primary key (news_version_id, image_id)
);

create table news (
       news_id serial not null primary key,
       news_version_id integer not null unique references news_versions,
       "date" date not null
);

create view drafts as
       select news_versions.*
       from news_versions left join news using (news_version_id)
       where news_id is null;

commit;
