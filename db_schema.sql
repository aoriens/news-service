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

-- Images are shared among news and should be considered immutable. On
-- deleting an image one should check whether other tables refer to
-- it.
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
       token_hash bytea not null,

       -- A marker of a deleted user.
       --
       -- Why not deleting a user indeed? Authors and comments depend
       -- on users directly. We would have to make user_id references
       -- nullable, which is worse for data integrity, and comments
       -- already use NULL user_id to designate an anonymous user,
       -- which can behave not like a deleted user. We would create a
       -- fake "deleted" user, but it is hardly easier. According to
       -- the second approach, we would have to update a lot of author
       -- and comment rows before deleting a user, which can be slow
       -- sometimes. Marking a user with the deleted flag looks the
       -- best in terms of data integrity, simplicity, and
       -- performance.
       --
       -- A deleted user entity should be considered truly deleted
       -- from the user's point of view. No information of the user
       -- entity can leak to the user, except the fact that there was
       -- a user formerly and it has been deleted. Entitites dependent
       -- on such a user entity can stay visible to the user. Although
       -- it is possible to recover a deleted user, it is not a
       -- purpose of the decision.
       is_deleted boolean not null default false
);

create view extended_users as
select *,
       coalesce(users.first_name || ' ' || users.last_name, users.last_name) as first_to_last_name
from users;

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
as $$
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
       -- A null author means that the author is deleted.
       author_id integer references authors on delete set null,
       -- A null category means an unspecified or a deleted category.
       category_id integer references categories,
       main_photo_id integer references images,
       created_from_news_id integer references news(news_id)
);

create table news_versions_and_tags_relation (
       news_version_id integer not null references news_versions on delete cascade,
       tag_id integer not null references tags on delete cascade,
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

create table comments (
       comment_id serial not null primary key,
       news_id integer not null references news,
       -- NULL means anonymous user
       user_id integer references users,
       text varchar not null,
       created_at timestamp with time zone not null
);

commit;
