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

To start development quickly, you can use `development.conf` file. You may
accommodate it for your needs or import it to your own configuration file, e.g.
`config.private`:

    import "development.conf"

    postgresql {
      # Overriding parameters specified in development.conf
    }

`*.private` files are ignored by git, so it is safe to give such a name to a
configuration file containing passwords.

# API

## Entity encoding

Non-empty request and response bodies containing API entities are encoded in
JSON, when the opposite is not specified.

## Pagination

Responses returning lists of entities support paginated output. It is controlled
with parameters `offset` and `limit`. They can be passed in the URI query.

`limit` is a number meaning the amount of entities to output in a single
response. When missing or too big, the maximum configured limit is used. It must
not be negative.

`offset` is a number of the first entity to output which defaults to `0`. It
must not be negative.

## Endpoints

### `GET /image/{image_id}`

Returns an image at the specified URL. The endpoint is not considered as part of
the public API, it is used for constructing URLs returned by other endpoints.

The response contains the image data with the corresponding MIME type.

### `GET /news`

Returns a list of [News](#News) entities.

### `POST /user/create`

Creates a user. Accepts [InUser](#InUser) entity in the request body and returns [User](#User) entity.

Returns a list of [News](#News) entities.

### `GET /user/{user_id}`

Returns a [User](#User).

## Entities

### Day

A string in `YYYY-mm-dd` format to specify a calendar day.

### InImage

A request to create an image. Fields:

- `base64_data` - a base64-encoded image data. A string, required.
- `content_type` - a MIME content type of the image. A string, required.

### InUser

An incoming request to create a user. Fields:

- `first_name` - the user's first name. A string, optional.
- `last_name` - the user's last name. This is to be used in case of a
  single-component name. A string, required.
- `avatar` - the user's avatar image. An [InImage](#InImage), optional.

### News

A news entry. Fields:

- `id` - the entity identifier. An integer, required.
- `title` - the news title. A string, required.
- `date` - the issue date. A [Day](#Day), required.
- `text` - the news body text. It is considered as a plain Unicode text.
  A string, required.

### UTCTime

A string in ISO8601 format to describe a specific UTC date and time. Example:
`2020-08-29T08:04:52Z`.

### User

A user. Fields:

- `id` - the user's identifier. An integer, required.
- `first_name` - the first name. A string, optional.
- `last_name` - the last name. A string, required.
- `avatar_url` - the avatar image URL. An string, optional.
- `created_at` - the time the user was created. A [UTCTime](#UTCTime), required.
- `is_admin` - whether the user have administrator permissions. A boolean,
  required.
- `secret_token` - the authentication token. The field is only output when
  creating a user, otherwise it is missing. A string, optional.
