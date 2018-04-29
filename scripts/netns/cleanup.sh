#!/usr/bin/env bash

set -e
set -o pipefail

## In case when something went wrong, allows to remove all changes introduced
## during network namespaces manipulation.

if [[ $EUID > 0 ]]; then
    echo "Need to be root!"
    exit 1
fi

gw_eth="mem-gw"

for no in {0..16}; do
    netns="mem$no"
    ip netns delete $netns 2> /dev/null || :

    gw_eth="mem-gw$no"
    ip link delete $gw_eth 2> /dev/null || :
done;

ip link delete $gw_eth 2> /dev/null || :
