#!/usr/bin/env bash

main() {
    local num=$1
    local output=""
    if [ $(( $num % 3 )) == "0" ]; then
        output="${output}Pling"
    fi
    if [ $(( $num % 5 )) == "0" ]; then
        output="${output}Plang"
    fi
    if [ $(( $num % 7 )) == "0" ]; then
        output="${output}Plong"
    fi
    if [ "$output" == "" ]; then
        output="$num"
    fi
    echo "$output"
}

main "$@"
