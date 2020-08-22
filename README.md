# news-service

This is a training project - a simple news service with RESTful interface,
written in Haskell. It's going to support getting and publishing news, several
kinds of users, authentication etc. It uses warp and PostgreSQL.

# Building

Run `stack build`.

# Running

    news-service --config PATH_TO_CONFIG

A sample, documented configuration file is available in
`news-service.default.conf`.

To start development quickly, you can use file `development.conf`. You may
accommodate it for your needs or import it to your own configuration file, e.g.
`config.private`:

    import "development.conf"

    postgresql {
      # Overriding parameters specified in development.conf
    }

Files `*.private` are ignored by git, so it is safe to give such a name to a
configuration file containing passwords.

# API

## Pagination

Responses returning lists of entities support paginated output which can be
managed with parameters `offset` and `limit`. They can be passed in the URI
query.

`limit` is a number meaning the amount of entities to output in a single
response. When missing or too big, the maximum configured limit is used. It must
not be negative.

`offset` is a number of the first entity to output which defaults to `0`. It
must not be negative.

## Endpoints

### `GET /news`

Returns a list of [News](#News) entities.

## Entities

### News

A news entry. Fields:

- `id` - the entity identifier
- `title` - the news title
- `date` - the issue date
- `text` - the news body text. It is considered as a plain Unicode text.
