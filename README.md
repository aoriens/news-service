# news-service

This is a training project - a simple news service with RESTful interface,
written in Haskell. It's going to support getting and publishing news, several
kinds of users, authentication etc. It uses warp and PostgreSQL.

# Building

Run `stack build`.

# Running

    news-service --config PATH_TO_CONFIG

A sample, documented configuration file is available in
`news-service.conf.default`.

To start development quickly, you can use file `development.conf`. You may
accommodate it for your needs or import it to your own configuration file, e.g.
`config.private`:

    import "development.conf"

    postgresql {
      # Overriding parameters specified in development.conf
    }

Files `*.private` are ignored by git, so it is safe to give such a name to a
configuration file containing passwords.
