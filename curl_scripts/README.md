# Test scripts

The directory contains scripts to quickly run some predefined requests. You need
curl to be installed.

# Usage

Normally, scripts does not require parameters. The following command sends `GET
<DOMAIN>/news`:

    ./get_news

Scripts can support an optional query parameter. Note that it is added to the
URL without escaping. The following command sends `GET
<DOMAIN>/news?offset=10&limit=20`:

    ./get_news "?offset=10&limit=20"

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
