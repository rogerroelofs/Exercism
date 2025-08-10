#!/usr/bin/env bash

function main() {

    declare -a words
    IFS=' -_*' read -r -a words <<<"${1^^}"

    for word in $words; do
        word=`echo $word | sed -e 's/^[[:space:]_]*//' | tr a-z A-Z`
        echo -n ${word:0:1}
    done
    echo
}

main "$@"
