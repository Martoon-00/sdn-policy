#!/usr/bin/env bash

set -e
set -o pipefail

## In case when something went wrong, allows to remove all changes introduced
## during network namespaces manipulation.

if [[ $EUID > 0 ]]; then
    echo "Need to be root!"
    exit 1
fi

br="membr"  # name of bridge

for no in {0..16}; do
    netns="mem$no"
    ip netns delete $netns 2> /dev/null || :

    tap_eth="memtap$no"
    ip link delete $tap_eth 2> /dev/null || :
done;

ifconfig $br down 2> /dev/null || :
brctl delbr $br 2> /dev/null || :
