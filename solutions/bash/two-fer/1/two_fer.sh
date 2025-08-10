#!/usr/bin/env bash

main () {
    local YOU=${1:-"you"}
    echo "One for $YOU, one for me.";
}

# call main with all of the positional arguments
main "$@"
