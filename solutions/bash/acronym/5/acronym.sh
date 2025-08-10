#!/usr/bin/env bash

function main() {
    set -f
    IFS=" -"
    for word in $1; do
        word=$(echo "$word" | sed -e 's/^[[:space:]_\*]*//' | tr "[:lower:]" "[:upper:]")
        echo -n "${word:0:1}"
    done
    echo
}

main "$1"
