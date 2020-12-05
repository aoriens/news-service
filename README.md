# news-service

This is a training project - a simple news service with a REST-like interface,
written in Haskell. It's going to support getting and publishing news, several
kinds of users, authentication etc. It uses warp and PostgreSQL.

# Building

Run `stack build`.

# Setup

1. Install PostgreSQL.
2. Create a database (it is called `news` here, but you may choose a different
   name):

```sh
createdb news
psql news -f db_schema.sql
```

3. You may populate the database with test data. Beware that it will erase old
   data.

```sh
psql news -f test_data.sql
```

4. Create a configuration file. A sample, documented configuration file is
   available in `news-service.default.conf`. To start development quickly, you
   can use `development.conf` file. You may accommodate it for your needs or
   import it to your own configuration file, e.g. `config.private`. `*.private`
   files are ignored by git, so it is safe to give such a name to a
   configuration file containing passwords for development:

```
import "development.conf"

postgresql {
  # Overriding parameters specified in development.conf
}
```

# Running

```sh
news-service --config PATH_TO_CONFIG
```

# API

See [API documentation](documentation/API.md).

# Development

`curl_scripts` directory contains curl scripts to test supported requests
quickly.

`test_data.sql` contains an administrator user with the least authentication
token possible, which helps to run requests which require authentication. The
token corresponds to the pattern of `<USERID>,`, e.g. `1,`, if you populate the
database with the test data right after creating it.
