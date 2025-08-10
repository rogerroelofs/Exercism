#!/usr/bin/env bash

main() {
    if [ "$#" != 2 ]; then
        echo "Usage: hamming.sh <string1> <string2>"
        exit -1
    fi
    if [ ${#1} -ne ${#2} ]; then
        echo "strands must be of equal length"
        exit -1
    fi
    local first=$1
    local sec=$2
    local dist=0
    for (( i=0; i<${#first}; i++ )); do
        if [ "${first:$i:1}" != "${sec:$i:1}" ]; then
            dist=$((dist+1))
        fi
    done
    echo "$dist"
}

main "$@"
