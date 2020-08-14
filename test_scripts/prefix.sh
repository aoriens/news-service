# A shell library file to be included into all test scripts.

: ${DOMAIN=localhost:3000}
QUERY=${1-}

# The function may be used in test scripts. It uses HTTP method $1 and
# URL $2 without the scheme and domain part:
#
# send GET news     ;# does GET request to <myserver.mydomain>/news
send() {
    url="$(make_url "$2" "$QUERY")"
    echo ">>> Requesting $url"
    curl -X "$1" -i "$url"
}

# Parameters: URI path, additional URI query
make_url() {
    # If $1 starts from /, it will be appended to the domain.
    # Otherwise an additional slash is to be inserted.
    if test "${1#/}" != "$1"
    then
        maybe_slash=
    else
        maybe_slash=/
    fi
    echo "$DOMAIN$maybe_slash$1$2"
}
