#!/usr/bin/env bash
set -e
set -o pipefail

## This script creates a new network namespace and runs shell (zsh by default)
## under it. First and the only argument designates id of network namespace,
## allowing to launch different applications from different subnetworks
## and thus easily track their traffic separatelly via, say, tcpdump.
##
## Once entering subnet, one will have a single network interface to communicate
## with main network via its $netns interface and also with other subnetworks.
##
## Based on this instruction:
## https://askubuntu.com/questions/11709/how-can-i-capture-network-traffic-of-a-single-process
##
## NOTE: Subnetwork may shortly lost connection with main network due to
## dhclient dropping statically assigned IP. To overcome that, add to
## /etc/network/interfaces the following line:
## > iface $gw_eth inet static
## (definition of $gw_eth see below). This will make NetworkManager, and thus
## dhclient, ignore specified interface.


# Check for root
if [[ $EUID > 0 ]]; then
    echo "Need to be root!"
    exit 1
fi

# Get id
no=$1
if [[ $no == '' ]]; then
    echo "Specify subnetwork id!"
    exit 1
fi

# Whether need to establish network namespace; connects to existing one when
# 'false'
configure=1

# Parse flags
OPTIND=1
while getopts "n" opt "${@:2}"; do
    echo $opt
    case "$opt" in
        n) configure=0
           echo "No configure"
           ;;
    esac
done
shift $((OPTIND-1))

# Set shell used inside subnet
if [[ $SHELL == '' ]]; then
    SHELL="zsh"
fi

netns="mem$no"
eth="mem$no"
gw_eth="mem-gw$no"
subnet="30.160.$((60+$no))"
ip=$subnet.50
gw_ip=$subnet.254

if [[ $configure == 1 ]]; then
    # Add namespace
    ip netns add $netns

    # Add interface to namespace
    ip link add $eth type veth peer name $gw_eth
    # Set active namespace for the interface
    ip link set $eth netns $netns

    # Assign IPs
    ip netns exec $netns ifconfig $eth up $ip netmask 255.255.255.0
    ifconfig $gw_eth up $gw_ip netmask 255.255.255.0

    # Add route between new namespace and main network
    ip netns exec $netns route add default gw $gw_ip dev $eth
fi

# Running shell under network namespace

clear
echo "== Entered virtual network =="
ip netns exec $netns $SHELL \
    || :  # even if shell exists with nonzero, do not fail this script

clear
echo "Exited virtual network"

if [[ $configure == 1 ]]; then
    # cleanup
    ip netns delete $netns
    ip link delete $gw_eth
fi
