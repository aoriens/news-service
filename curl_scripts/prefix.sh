# A shell library file to be included into all test scripts.

: ${DOMAIN=localhost:3000}

run_curl() {
    echo "curl -i $@"
    curl -i "$@"
}

die () {
    echo "$@" 2>&1
    exit 1
}

require_token () {
    if test X"${TOKEN+1}" = X
    then
        die "\
You need to specify TOKEN variable containing the authentication token value:
\$ TOKEN=mytoken $0 ARGS... "
    fi
}

allow_token () {
    if test X"${TOKEN+1}" = X
    then
        echo "\
Note: you may specify TOKEN variable containing the authentication token value:
\$ TOKEN=mytoken $0 ARGS...
It is not needed, though, but it can affect operation.
"
    fi
}

disallow_Q() {
    if test -n "${Q+1}"
    then
        die "The Q variable is not supported for passing URI query"
    fi
}
