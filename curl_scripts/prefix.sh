# A shell library file to be included into all test scripts.

: ${DOMAIN=localhost:3000}

run_curl() {
    if test -z "${TOKEN+1}"
    then
        run_curl_auth=
    else
        run_curl_auth="-u $TOKEN:"
    fi
    
    echo "curl -i $@ $run_curl_auth"
    curl -i "$@" $run_curl_auth
}

die () {
    echo "$@" 2>&1
    exit 1
}

allow_token () {
    if test -z "${TOKEN+1}"
    then
        echo "\
Note: you may specify TOKEN variable containing the authentication token value:
    \$ TOKEN=mytoken $0 ARGS...
"
    fi
}

disallow_Q() {
    if test -n "${Q+1}"
    then
        die "The Q variable is not supported for passing URI query"
    fi
}
