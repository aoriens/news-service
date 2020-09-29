# news-service

This is a training project - a simple news service with RESTful interface,
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

3. You may populate the database with test data. It will erase old data, but you
   make backups regularly, so this won't be an issue.

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

# Development

`curl_scripts` directory contains curl scripts to test supported requests
quickly.

`test_data.sql` contains an administrator user with the least authentication
token possible, which helps to run requests which require authentication. The
token corresponds to the pattern of `<USERID>,`, e.g. `1,`, if you populate the
database with the test data right after creating it.

# API

## Authentication

Some parts of API require authentication, which is documented appropriately, but
majority of methods is available without authentication. Currently we support
HTTP basic authentication. You should use user's secret token as a login and an
empty password. The secret token is only returned on user creation.

In case of authentication failure or lack of privileges `404 NotFound` is
returned in order to hide API which requires additional privileges.

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

### `GET /authors`

Returns a list of [Author](#Author) entities. Requires the administrator
privilege.

### `POST /authors`

Creates an author. Accepts [InAuthor](#InAuthor) entity in the request body and
returns [Author](#Author) entity. Requires the administrator privilege.

### `DELETE /authors/{author_id}`

Deletes the specified [Author](#Author) and returns no content. Requires the
administrator privilege.

### `GET /authors/{author_id}`

Returns the specified [Author](#Author). Requires the administrator privilege.

### `PATCH /authors/{author_id}`

Accepts [UpdatedAuthor](#UpdatedAuthor) entity, updates the corresponding
[Author](#Author) entity and returns the updated representation. Requires the
administrator privilege.

### `POST /categories`

Creates a (possibly nested) category. Accepts [InCategory](#InCategory) entity
in the request body and returns [Category](#Category) entity. Requires the
administrator privilege.

### `GET /categories/{category_item_id}`

Returns a [Category](#Category) comprising hierarchy of [category
items](#CategoryItem) up to the item with the specified identifier.

### `GET /images/{image_id}`

Returns an image at the specified URL. The endpoint is not considered as part of
the public API, it is used for constructing URLs returned by other endpoints.

The response contains the image data with the corresponding MIME type.

### `GET /news`

Returns a list of [News](#News) entities.

### `GET /users`

Returns an array of [User](#User) entities.

### `POST /users`

Creates a user. Accepts [InUser](#InUser) entity in the request body and returns
the created [User](#User) entity.

### `DELETE /users/{user_id}`

Deletes the identified user and returns no content. Requires authentication of a
user having administrator privilege.

### `GET /users/{user_id}`

Returns the specified [User](#User).

## Entities

### Author

An author of news. Fields:

- `author_id` - the identifier of the author. An integer, required.
- `user` - the corresponding user. A [User](#User), required.
- `description` - the author description. A string, required.

### Category

A news category. This is a non-empty array of [CategoryItem](#CategoryItem)
objects, logically nested, starting from the most significant one.

### CategoryItem

A part of a hierarchical news category. Each category item has either a parent
item or no one, which is represented by the parent-to-child order of elements in
[Category](#Category). Fields:

- `category_item_id` - the identifier of a category item. An integer, required.
- `name` - the name. A string, required.

### Day

A string in `YYYY-mm-dd` format to specify a calendar day.

### InAuthor

A request to create an author. Fields:

- `user_id` - the identifier of existing [User](#User). An integer, required.
- `description` - the description of the author. A string, required.

### InCategory

A request to create categories. Fields:

- `names` - names of categories to create and nest subsequently in the
  parent-to-child order. This is an array of non-empty strings, required. It
  must contain at least one element. Example: `["fp", "haskell", "ghc"]` will
  result in creating `fp` category containing just created `haskell` category
  containing just created `ghc` category.
- `parent_category_item_id` - the identifier of an existing
  [CategoryItem](#CategoryItem) where a new category will be created. When no
  one specified, a new root category will be created. An integer, optional.

### InImage

A request to create an image. Fields:

- `base64_data` - a base64-encoded image data. A string, required.
- `content_type` - a MIME content type of the image. A string, required.

### InUser

An request to create a user. Fields:

- `first_name` - the user's first name. A string, optional.
- `last_name` - the user's last name. This is to be used in case of a
  single-component name. A string, required.
- `avatar` - the user's avatar image. An [InImage](#InImage), optional.

### News

A news entry. Fields:

- `news_id` - the entity identifier. An integer, required.
- `title` - the news title. A string, required.
- `date` - the issue date. A [Day](#Day), required.
- `text` - the news body text. It is considered as a plain Unicode text.
  A string, required.

### UTCTime

A string in ISO8601 format to describe a specific UTC date and time. Example:
`2020-08-29T08:04:52Z`.

### User

A user. Fields:

- `user_id` - the user's identifier. An integer, required.
- `first_name` - the first name. A string, optional.
- `last_name` - the last name. A string, required.
- `avatar_url` - the avatar image URL. An string, optional.
- `created_at` - the time the user was created. A [UTCTime](#UTCTime), required.
- `is_admin` - whether the user has administrator privilege. A boolean,
  required.
- `secret_token` - the authentication token. The field is only output when
  creating a user, otherwise it is missing. A string, optional.

### UpdatedAuthor

An instruction to update an [Author](#Author). Fields:

- `description` - the author's new description. A string, required.
