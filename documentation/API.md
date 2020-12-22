# API

## Authentication

Some parts of API require authentication, which is documented appropriately, but
majority of methods does not. Currently we support HTTP basic authentication.
You should use the user's secret token as a login and an empty password. The
secret token is only returned on user creation.

In case of authentication failure or lack of privileges `404 NotFound` may be
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
entity, and returns the updated [Author](#Author) representation. Requires the
administrator privilege.

### `GET /authors/{author_id}/drafts`

Returns the list of [Draft](#Draft) entities created by an author identified
with `{author_id}`. It is only allowed to the user related to the specified
author. Requires authentication.

### `GET /categories`

Returns a list of [CategoryAncestry](#CategoryAncestry) entities for each
existing [Category](#Category).

### `POST /categories`

Creates a (possibly nested) category. Accepts [CreateCategory](#CreateCategory)
entity in the request body and returns a [CategoryAncestry](#CategoryAncestry)
entity up to the deepest created category. Requires the administrator privilege.

### `GET /categories/{category_id}`

Returns a [CategoryAncestry](#CategoryAncestry) comprising hierarchy of
[categories](#Category) up to the category with identifier `{category_id}`.

### `DELETE /categories/{category_id}`

Deletes the identified category and returns no content. Requires the
administrator privilege. Drafts and news residing in the category or any
descendant subcategory will be moved to the parent category. If the category to
be deleted is a root category, the drafts and the news will be made
uncategorized.

### `PATCH /categories/{category_id}`

Modifies a category. Accepts an [UpdateCategory](#UpdateCategory) entity in the
request body and returns the [CategoryAncestry](#CategoryAncestry) for the
updated [Category](#Category). Requires the administrator privilege.

### `DELETE /comments/{comment_id}`

Deletes the [Comment](#Comment) identified with `{comment_id}` and returns no
content. It requires authentication. It is only allowed to delete a comment to
an administrator user or a user who posted the comment. Anonymous comments can
only be deleted by an administrator.

### `GET /comments/{comment_id}`

Returns the [Comment](#Comment) identified with `{comment_id}`.

### `GET /drafts`

Returns the list of [Draft](#Draft) entities created by all authors related to
the authenticated user. Requires authentication.

### `POST /drafts`

Creates a draft version of news. Accepts [CreateDraft](#CreateDraft) entity in
the request body and returns [Draft](#Draft) entity. The method requires
authentication. Your cannot perform the operation on behalf of an
[Author](#Author) that you do not own.

### `DELETE /drafts/{draft_id}`

Deletes the [Draft](#Draft) entity identified with `{draft_id}` and returns no
content. It requires authentication. You must be a user related to the author of
the draft, and the draft must exist.

### `GET /drafts/{draft_id}`

Returns a [Draft](#Draft) entity identified with `{draft_id}`. It requires
authentication. You must be a user related to the author of the draft.

### `PATCH /drafts/{draft_id}`

Modifies the [Draft](#Draft) entity identified with `{draft_id}` and returns the
updated [Draft](#Draft) entity. Accepts [UpdateDraft](#UpdateDraft) entity in
the request body which describes modifications to do. It requires
authentication. You must be a user related to the author of the draft.

### `POST /drafts/{draft_id}/publish`

Publishes a [Draft](#Draft) with the given identifier as a news article. Returns
a created or updated [News](#News) entity. The published draft is deleted then.
It requires authentication. You need to be the owner of the draft.

### `GET /images/{image_id}`

Returns an image at the specified URL. The method is not considered as part of
the public API, it is used for constructing URLs returned by other methods.

The response contains the image data with the corresponding MIME type.

### `GET /news`

Returns a list of [News](#News) entities.

#### Sorting

The following URI query parameters may be passed to affect sort order:

- `sort` - the sort key. When missing, it is assumed to be `date`. The following
  values are accepted:
  - `date` - sort by news article date.
  - `author` - sort by the author name, the last name first.
  - `num_photos` - sort by the number of photos in the news.
- `reverse_sort` - a flag to reverse the sort order. The parameter value is
  ignored. Multiple occurrences are treated as if the parameter is used once.

#### Filtering

The following URI query parameters may be passed to filter the list of articles:

- `q` - a substring to search everywhere: the article title, body, author name,
  tags, category. The search is case-insensitive. A news article is output if at
  least one of the fields contains the substring.
- `date` - a date or a date range when news is published. The parameter may be
  used many times to specify multiple dates or date ranges. The following
  formats are accepted:
  - `YYYY-mm-dd` - a specific day in `ISO8601` date format.
  - `YYYY-mm-dd,YYYY-mm-dd` - an inclusive date range. The start or the end date
    may be absent, but not both, to select all dates since/until a specific day.
- `author_id` - an integer identifier of an [Author](#Author) of the news or a
  comma-separated list thereof. The parameter may be passed many times to
  specify multiple values or value lists to be joined.
- `author` - a substring of a [User](#User) name who is an [Author](#Author) of
  the news using case-insensitive match. The parameter may be passed many times
  to specify multiple values.
- `category_id` - an integer identifier of a [Category](#Category) of the news,
  or of some ancestor category of it, or a comma-separated list thereof. The
  parameter may be passed many times to specify multiple values or value lists
  to be joined.
- `category` - a substring of a [Category](#Category) name of the news, or of
  some ancestor category of it. The case-insensitive match is used. The
  parameter may be passed many times to specify multiple values.
- `tag_id` - an integer identifier of a [Tag](#Tag) that a news article is
  tagged with, or a comma-separated list thereof. A news article needs to be
  tagged with ANY tag specified in order to be output. The parameter may be
  passed many times to specify multiple values or value lists to be joined.
- `tag` - a substring of a [Tag](#Tag) name that a news article is tagged with.
  The case-insensitive match is used. The parameter may be passed many times to
  specify multiple values. A news article needs to be tagged with ANY tag
  matching the parameter in order to be output.
- `required_tag_id` - an integer identifier of a [Tag](#Tag) that a news article
  is tagged with, or a comma-separated list thereof. A news article needs to be
  tagged with ALL tags specified in order to be output. The parameter may be
  passed many times to specify multiple values or value lists to be joined.
- `required_tag` - a substring of a [Tag](#Tag) name that a news article is
  tagged with. The case-insensitive match is used. The parameter may be passed
  many times to specify multiple values. For EACH parameter value passed there
  must be a matching tag related to a news article in order for the news article
  to be output.
- `title` - a substring in the news title using the case-insensitive match. The
  parameter may be passed many times to specify multiple values. It is enough
  for a news article to match at least one value in order to be output.
- `body` - a substring in the news body using the case-insensitive match. The
  parameter may be passed many times to specify multiple values. It is enough
  for a news article to match at least one value in order to be output.

The parameters are logically combined as follows:

- `date`
- AND (`author_id` OR `author`)
- AND (`category_id` OR `category`)
- AND (`tag_id` OR `tag`)
- AND (`required_tag_id` OR `required_tag`)
- AND `title`
- AND `body`
- AND `q`

If a parameter is missing, it should be excluded, as well as the binary operator
lacking the parameter.

### `GET /news/{news_id}`

Returns a [News](#News) article identified with `{news_id}`.

### `GET /news/{news_id}/comments`

Returns an array of [Comment](#Comment) entities for the news article identified
with `{news_id}`.

### `POST /news/{news_id}/comments`

Accepts a [CreateComment](#CreateComment) entity in the request body, creates
comment, and returns a [Comment](#Comment) entity just created. If the user is
authenticated, they will be saved as the comment author, otherwise the comment
will be posted anonymously.

### `GET /news/{news_id}/drafts`

Returns [Draft](#Draft) entities created from the news identified with
`{news_id}`. Such drafts may be created using the same URI with `GET` method. It
requires authentication. Only the author of the news article may perform the
operation.

### `POST /news/{news_id}/drafts`

Creates a [Draft](#Draft) of news identified with `{news_id}` and returns it.
The new draft is identical to the news content. It may be edited and published
to update the news article.

### `GET /tags`

Returns a list of [Tag](#Tag) entities.

### `POST /tags`

Creates a tag. Accepts [CreateTag](#CreateTag) entity in the request body and
returns either a created or existing [Tag](#Tag) entity. Each tag must have a
unique, non-empty name. If a tag with such a name exists, no new tag will be
created and the existing tag is returned.

### `DELETE /tags/{tag_id}`

Deletes a [Tag](#Tag) identified with `{tag_id}` and returns no content. You
must be an administrator user.

### `GET /tags/{tag_id}`

Returns the specified [Tag](#Tag).

### `PATCH /tags/{tag_id}`

Accepts [UpdateTag](#UpdateTag) entity, updates the [Tag](#Tag) identified with
`{tag_id}`, and returns the updated tag. Requires the administrator privilege.

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
- `user` - the corresponding user. It is either a [User](#User) or a string
  `DELETED`, if the user is deleted. This is a required field.
- `description` - the author description. A string, required.

### CategoryAncestry

A news category ancestry. This is an array of [Category](#Category) entities,
logically nested, starting from a root one down to the specific category. The
array may be empty to designate an unspecified (over-root) category.

### Category

A news category. Each category has either a parent one or no one, which is
represented by the parent-to-child order of elements in the corresponding
[CategoryAncestry](#CategoryAncestry). Fields:

- `category_id` - the identifier of the category. An integer, required.
- `name` - the name. A string, required.

### Day

A string in `YYYY-mm-dd` format to specify a calendar day.

### Comment

A comment posted by a user for a news article. Fields:

- `comment_id` - the identifier of a comment. An integer, required.
- `news_id` - the identifier of a commented [News](#News) article. An integer,
  required.
- `text` - the comment body text. A string, required.
- `author` - a user who created the comment. It may be a [User](#User) entity,
  string `ANONYMOUS` if the comment is posted anonymously, or string `DELETED`
  if the comment is posted by a deleted user. This is a required field.
- `created_at` - date and time time the comment is posted at. A
  [UTCTime](#UTCTime), required.

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
- `parent_id` - the identifier of an existing [Category](#Category) which will
  be the parent of a new created category. When no one specified, a new root
  category will be created. An integer, optional.

### CreateComment

A request to create a comment for a news article. Fields:

- `text` - the comment body text. A string, required.

### CreateDraft

A request to create a news draft. Fields:

- `title` - the document title. A string, required.
- `text` - the document body as plain text. A string, required.
- `author_id` - an identifier of an [Author](#Autor) of the news. It is optional
  and may be inferred automatically, if you have exactly one author, otherwise
  it is required. An integer, optional.
- `category_id` - an identifier of a [Category](#Category) of the news. An
  integer, optional.
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

A draft of a news article. Fields:

- `draft_id` - an identifier of the draft. An integer, required.
- `title` - the news title. A string, required.
- `text` - the news body text. It is considered as a plain Unicode text.
  A string, required.
- `author` - the draft author. An [Author](#Author) entity or string `DELETED`,
  if the author entity is deleted. The field is required.
- `category` - the news category. A [CategoryAncestry](#CategoryAncestry),
  required.
- `photo` - the main illustration photo URI for the news. A string, required.
- `photos` - additional illustration URIs. An array of strings, required.
- `tags` - tags for the news. An array of [Tag](#Tag) objects, required.

### News

A news article. Fields:

- `news_id` - the entity identifier. An integer, required.
- `title` - the news title. A string, required.
- `date` - the issue date. A [Day](#Day), required.
- `text` - the news body text. It is considered as a plain Unicode text.
  A string, required.
- `author` - the news author. An [Author](#Author) entity or string `DELETED`,
  if the news author is deleted. The field is required.
- `category` - the news category. A [CategoryAncestry](#CategoryAncestry),
  required.
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

### UpdateCategory

An instruction to update a [Category](#Category). Fields:

- `name` - a new name of the category. This is an optional string that must not
  be null. The category name must be non-empty and unique among sibling
  categories.
- `parent_id` - an identifier of the new parent [Category](#Category). This is
  an optional number. The null value means making the category a root one.
  Changing the category parent must not create an ancestry loop.

### UpdateDraft

A request to modify a news draft. Its fields describe modifications to do.
Omission of a field means keeping the old value of the field unchanged:

- `title` - a new document title. This is an optional string that must not be
  null.
- `text` - a new document body as plain text. This is an optional string that
  must not be null.
- `category_id` - a new identifier of a [Category](#Category) of the news or
  `null` to designate setting no category. This is optional.
- `photo` - a new primary illustration for the news. This is a URI of an image
  returned by the service, a [CreateImage](#CreateImage) entity, or `null` to
  designate deleting the illustration. This is optional.
- `photos` - a new list of additional illustrations for the news. This is an
  optional array consisting of URIs of images returned by the service and
  [CreateImage](#CreateImage) entities. This is optional, but must not be
  `null`. Duplicate image URLs will be skipped.
- `tag_ids` - a new list of tags of the news. This is an optional array of
  integer identifiers of [Tag](#Tag) entities which must not be null. Duplicate
  items are allowed and skipped.

### UpdateTag

An instruction to update a [Tag](#Tag). Fields:

- `name` - a new name of the tag. A string, required.
