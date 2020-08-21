begin;

update config set value = '3' where key = 'schema_version';

alter table users drop token_hash_algorithm;

update users
set token_hash = cast('\x00' as bytea) || token_hash;

commit;
