#!/usr/bin/env bash

main() {
    local num=$1
    local output=""
    if [ $(expr $num % 3) == "0" ]; then
        output="${output}Pling"
    fi
    if [ $(expr $num % 5) == "0" ]; then
        output="${output}Plang"
    fi
    if [ $(expr $num % 7) == "0" ]; then
        output="${output}Plong"
    fi
    if [ "$output" == "" ]; then
        output="$num"
    fi
    echo $output
}

main "$@"
