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

## Methods

### `GET /authors`

Returns a list of [Author](#Author) entities. Requires the administrator
privilege.

### `POST /authors`

Creates an author. Accepts [CreateAuthor](#CreateAuthor) entity in the request
body and returns [Author](#Author) entity. Requires the administrator privilege.

### `DELETE /authors/{author_id}`

Deletes the specified [Author](#Author) and returns no content. Requires the
administrator privilege.

### `GET /authors/{author_id}`

Returns the specified [Author](#Author). Requires the administrator privilege.

### `PATCH /authors/{author_id}`

Accepts [UpdateAuthor](#UpdateAuthor) entity, updates the corresponding author
entity and returns the updated [Author](#Author) representation. Requires the
administrator privilege.

### `GET /categories`

Returns a list of [Category](#Category) entities.

### `POST /categories`

Creates a (possibly nested) category. Accepts [CreateCategory](#CreateCategory)
entity in the request body and returns [Category](#Category) entity. Requires
the administrator privilege.

### `GET /categories/{category_item_id}`

Returns a [Category](#Category) comprising hierarchy of [category
items](#CategoryItem) up to the item with the specified identifier.

### `DELETE /categories/{category_id}`

Deletes the identified category and returns no content. Only the least
significant [CategoryItem](#CategoryItem) of the category will be deleted.
Requires authentication of a user having the administrator privilege.

### `POST /drafts`

Creates a draft version of news. Accepts [CreateDraft](#CreateDraft) entity in
the request body and returns [Draft](#Draft) entity. The method requires
authentication. Your cannot perform the operation on behalf of an
[Author](#Author) that you do not own.

### `GET /images/{image_id}`

Returns an image at the specified URL. The method is not considered as part of
the public API, it is used for constructing URLs returned by other methods.

The response contains the image data with the corresponding MIME type.

### `GET /news`

Returns a list of [News](#News) entities.

### `GET /tags`

Returns a list of [Tag](#Tag) entities.

### `POST /tags`

Creates a tag. Accepts [CreateTag](#CreateTag) entity in the request body and
returns either a created or existing [Tag](#Tag) entity.

### `GET /tags/{tag_id}`

Returns the specified [Tag](#Tag).

### `GET /users`

Returns an array of [User](#User) entities.

### `POST /users`

Creates a user. Accepts [CreateUser](#CreateUser) entity in the request body and
returns the created [User](#User) entity.

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

### CreateAuthor

A request to create an author. Fields:

- `user_id` - the identifier of existing [User](#User). An integer, required.
- `description` - the description of the author. A string, required.

### CreateCategory

A request to create categories. Fields:

- `names` - names of categories to create and nest subsequently in the
  parent-to-child order. This is an array of non-empty strings, required. It
  must contain at least one element. Example: `["fp", "haskell", "ghc"]` will
  result in creating `fp` category containing just created `haskell` category
  containing just created `ghc` category.
- `parent_category_item_id` - the identifier of an existing
  [CategoryItem](#CategoryItem) where a new category will be created. When no
  one specified, a new root category will be created. An integer, optional.

### CreateDraft

A request to create a news draft. Fields:

- `title` - the document title. A string, required.
- `text` - the document body as plain text. A string, required.
- `author_id` - an identifier of an [Author](#Autor) of the news. An integer,
  required.
- `category_id` - an identifier of an [Category](#Category) of the news. An
  integer, required.
- `photo` - the primary illustration for the news. This is an optional URL of an
  image returned by the service or an optional [CreateImage](#CreateImage)
  entity.
- `photos` - more illustrations for the news. This is an optional array
  consisting of URLs of images returned by the service and
  [CreateImage](#CreateImage) entities. Duplicate image URLs will be skipped.
- `tag_ids` - tags of the news. This is an optional array of integer identifiers
  of [Tag](#Tag) entities. Duplicate items are allowed and skipped.

### CreateImage

A request to create an image. Fields:

- `base64_data` - a base64-encoded image data. A string, required.
- `content_type` - a MIME content type of the image. A string, required.

### CreateTag

A request to create a tag. Fields:

- `name` - the tag name, which must not be empty. A string, required.

### CreateUser

A request to create a user. Fields:

- `first_name` - the user's first name. A string, optional.
- `last_name` - the user's last name. This is to be used in case of a
  single-component name. A string, required.
- `avatar` - the user's avatar image. A [CreateImage](#CreateImage), optional.

### Draft

A draft of a news entry. Fields:

- `draft_id` - an identifier of the draft. An integer, required.
- `title` - the news title. A string, required.
- `text` - the news body text. It is considered as a plain Unicode text.
  A string, required.
- `author` - the news author. An [Author](#Author), required.
- `category` - the news category. A [Category](#Category), required.
- `photo` - the main illustration photo URI for the news. A string, required.
- `photos` - additional illustration URIs. An array of strings, required.
- `tags` - tags for the news. An array of [Tag](#Tag) objects, required.

### News

A news entry. Fields:

- `news_id` - the entity identifier. An integer, required.
- `title` - the news title. A string, required.
- `date` - the issue date. A [Day](#Day), required.
- `text` - the news body text. It is considered as a plain Unicode text.
  A string, required.
- `author` - the news author. An [Author](#Author), required.
- `category` - the news category. A [Category](#Category), required.
- `photo` - the main illustration photo URI for the news. A string, required.
- `photos` - additional illustration URIs. An array of strings, required.
- `tags` - tags for the news. An array of [Tag](#Tag) objects, required.

### Tag

A news tag. Fields:

- `tag_id` - the entity identifier. An integer, required.
- `name` - the tag name. A string, required.

### UTCTime

A string in ISO8601 format to describe a specific UTC date and time. Example:
`2020-08-29T08:04:52Z`.

### User

A user. Fields:

- `user_id` - the user's identifier. An integer, required.
- `first_name` - the first name. A string, optional.
- `last_name` - the last name. A string, required.
- `avatar_url` - the avatar image URL. a string, optional.
- `created_at` - the time the user was created. A [UTCTime](#UTCTime), required.
- `is_admin` - whether the user has administrator privilege. A boolean,
  required.
- `secret_token` - the authentication token. The field is only output when
  creating a user, otherwise it is missing. A string, optional.

### UpdateAuthor

An instruction to update an [Author](#Author). Fields:

- `description` - the author's new description. A string, required.
