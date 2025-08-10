#!/usr/bin/env bash

function main() {
    local phrase=$1
    old_ifs="$IFS"
    IFS=" -"
    for word in $phrase; do
        word=`echo $word | sed -e 's/^[[:space:]_]*//' | tr a-z A-Z`
        echo -n ${word:0:1}
    done
    echo
}

main "$@"
