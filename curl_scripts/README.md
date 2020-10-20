# Test scripts

The directory contains scripts to quickly run some predefined requests. You need
curl to be installed.

# Usage

Normally, scripts do not require parameters, although some of them may do it.
The following command sends `GET <DOMAIN>/news`:

    ./get_news

Some scripts may support an optional query parameter. Note that it is appended
to the URL as is, without escaping. The leading '?' is also required. The
following command sends `GET <DOMAIN>/news?offset=10&limit=20`:

    ./get_news "?offset=10&limit=20"

Some methods require user authentication. You may specify the user
authentication token via `TOKEN` variable:

    TOKEN=1, ./get_authors

The default domain is `localhost:3000`, but you may override it via `DOMAIN`
variable:

    DOMAIN=example.com ./get_news

or longer:

    export DOMAIN=example.com
    ./get_news

# Development

All scripts should import `prefix.sh` in the first line of code, which allows
doing common configuration and parameter handling in a single place:

    . "$(dirname "$0")"/prefix.sh
