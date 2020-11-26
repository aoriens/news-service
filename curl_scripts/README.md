# Test scripts

The directory contains scripts to quickly run some predefined requests. You need
curl to be installed.

# Usage

Normally, scripts do not require parameters, although some of them may do it.
Run a script without parameters in order to get more usage information.

Scripts can use environment variables to accept some well-known, optional
parameters. You may run a script with the variables unset to get more info. To
pass a variable named `V` with value `1` to a script, run it in the following
way:

    # Sets the variable for the given invocation only
    V=1 ./some_script

    # Sets the variable for all subsequent invocations
    export V=1
    ./some_script

The following variables may be used:

- `TOKEN` - the authentication token of a user. It is accepted with some scripts
  which require or allow authentication.
- `Q` - the URI query. It is optional, but accepted with most scripts. The URI
  query is appended to the URI as is, so it must contain the leading question
  mark and be quoted properly: `Q=?limit=1`.
- `DOMAIN` - the URL domain and port. It is accepted with all scripts and
  defaults to `localhost:3000`.

# Development

All scripts should import `prefix.sh` in the first line of code, which allows
doing common configuration and parameter handling in a single place:

    . "$(dirname "$0")"/prefix.sh

You should send a request in the following way:

    run_curl "$DOMAIN/my/uri/path$Q"

The `$DOMAIN` part must always be used in order to allow changing the default
domain. The `$Q` part is also suggested to be used, since it is an easy way for
the user to test the request with an arbitrary URI query part, even when the
script does not support it. If the script does not support passing an arbitrary
URI query (say, the script forms the query on their own), invoke `disallow_Q`
function before sending the request.
