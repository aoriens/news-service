delete from news;

insert into news (title, body, "date") values
       ('GHC 14.0 has released', 'See ghc.haskell.org for details.', '2020-07-27'),
       ('New profunctors package has been available', 'See hackage.org', '2021-02-02');

insert into users (last_name, created_at, is_admin, token_hash, token_hash_algorithm)
values ('Admin', '2020-08-20', true, '\x00', 0)

