# A shell library file to be included into all test scripts.

: ${DOMAIN=localhost:3000}

# The function may be used in test scripts. It uses HTTP method $1 and
# URL $2 without the scheme and domain part:
#
# send GET news     ;# does GET request to <myserver.mydomain>/news
send() {
    curl -X "$1" -i "$(make_url "$2")"
}

make_url() {
    # If $1 starts from /, it will be appended to the domain.
    # Otherwise an additional slash is to be inserted.
    if test "${1#/}" != "$1"
    then
        echo "$DOMAIN$1"
    else
        echo "$DOMAIN/$1"
    fi
}
