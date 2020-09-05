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

