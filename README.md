# news-service

This is a training project - a simple news service with RESTful interface,
written in Haskell. It supports getting and publishing news, several kinds of
users, authentication etc. It uses warp and PostgreSQL.

# Building

Run `stack build`.

## Running

    news-service [--config PATH_TO_CONFIG]

If no configuration file is provided, default values will be used. A sample
configuration file is available in `news-service.config.default`.
