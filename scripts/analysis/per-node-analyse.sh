#!/usr/bin/env bash

input=$1
if [[ $input == '' ]]; then
    input='dump.log'
fi;

tmp="small-dump.log"

for i in {8..1..-1}; do
    echo "For $i-th process"
    port="800$i"
    grep $port $input > $tmp
    cat $tmp | wc -l
    ./sum-tcpdump.sh $tmp
done

rm $tmp
