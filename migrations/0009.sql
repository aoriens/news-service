begin;

update config set value = '9' where key = 'schema_version';

create table news_versions (
       news_version_id serial not null primary key,
       title varchar not null,
       body varchar not null
);

alter table news add news_version_id integer references news_versions;

do $$
declare
    r record;
    v_id integer;
begin
    for r in select news_id, title, body
             from news
    loop
        insert into news_versions (title, body)
        values (r.title, r.body)
        returning news_version_id
        into strict v_id;

        update news
        set news_version_id = v_id
        where news_id = r.news_id;
    end loop;
end;
$$;

alter table news
      alter news_version_id set not null,
      drop title,
      drop body;

commit;
