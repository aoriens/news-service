# A shell library file to be included into all test scripts.

: ${DOMAIN=localhost:3000}

run_curl() {
    echo "curl -i $@"
    curl -i "$@"
}
