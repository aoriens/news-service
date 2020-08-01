# Test scripts

The directory contains scripts to quickly run some predefined requests. You need
curl to be installed. Normally, scripts should not require any command line
parameters.

All scripts should import `prefix.sh` in the first line of code, which allows
doing common configuration in a single place:

    . "$(dirname "$0")"/prefix.sh
