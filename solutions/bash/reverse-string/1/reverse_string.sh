#!/usr/bin/env bash

set -f

main () {
    str="$1";
    rev="";
    len=${#str};
    for((i=$len-1;i>=0;i--)); do rev="$rev${str:$i:1}"; done;
    echo "$rev";
}

# call main with all of the positional arguments
main "$@"
