begin;

update config set value = '10' where key = 'schema_version';

alter table news_versions
      add author_id integer references authors,
      add category_id integer references categories,
      add main_photo_id integer references images;

do $$
declare
    default_user_id integer;
    default_author_id integer;
    default_category_id integer;
begin
    insert into users (last_name, created_at, token_hash)
    values (
           'default_user',
           now(),
           -- The empty token
           '\x00e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
    ) returning user_id into strict default_user_id;

    insert into authors (user_id, description)
    values (default_user_id, 'A default author')
    returning author_id into strict default_author_id;

    insert into categories (parent_id, name)
    values (null, 'other')
    returning category_id into strict default_category_id;

    update news_versions
    set author_id = default_author_id,
        category_id = default_category_id;
end;
$$;

alter table news_versions
      alter author_id set not null,
      alter category_id set not null;

commit;
