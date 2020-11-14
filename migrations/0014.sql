begin;

update config set value = '14' where key = 'schema_version';

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

commit;
