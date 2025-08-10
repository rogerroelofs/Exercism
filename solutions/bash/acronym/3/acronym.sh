#!/usr/bin/env bash

function main() {
    set -f
    old_ifs="$IFS"
    IFS=" -"
    for word in $1; do
        word=`echo $word | sed -e 's/^[[:space:]_\*]*//' | tr a-z A-Z`
        echo -n ${word:0:1}
    done
    echo
}

main "$1"
