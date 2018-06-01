#!/usr/bin/env bash

input=$1
if [[ $input == '' ]]; then
    input="dump.log"
fi;

cat $input \
    | awk '{print $11;}' \
    | sed '/^$/d' \
    | cut -d ':' -f 1 \
    | paste -s -d+ - \
    | bc
