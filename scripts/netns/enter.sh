#!/usr/bin/env zsh
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
## http://fosshelp.blogspot.ru/2014/08/connect-two-network-namespaces-using.html
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
if [[ $NS_SHELL == '' ]]; then
    NS_SHELL="zsh"
fi
SHELL=$NS_SHELL

netns="mem$no"  # name of namespace
eth="mem$no"  # name of interface into namespace
subnet="192.168.$((60))"  # IP of subnet belonging to the namespace
ip=$subnet.$no  # IP of interface under namespace
br="membr"  # name of bridge
tap_eth="memtap$no"  # name of interface, paired with $eth and connected to $br

if [[ $configure == 1 ]]; then
    # Establish bridge
    if [[ ! $(brctl show | grep $br || true) ]]; then
        echo "Bridge \"$br\" not found, configuring..."
        sudo brctl addbr $br
        sudo ip link set dev $br up
    fi

    # Add namespace
    ip netns add $netns

    # Bring up loopback interface - may be useful
    ip netns exec $netns ifconfig lo up

    # Add interface to namespace
    ip link add $eth type veth peer name $tap_eth
    # Move newly created interface into dedicated namespace
    ip link set $eth netns $netns

    # Connect interface with bridge
    brctl addif $br $tap_eth

    # Bring interfaces up
    ip netns exec $netns ip link set dev $eth up
    ip link set dev $tap_eth up

    # Assign IPs
    ip netns exec $netns ifconfig $eth up $ip netmask 255.255.255.0
fi

# Running shell under network namespace

clear
echo "== Entered virtual network $no =="
export ZSH_DISABLE_COMPFIX=true
ip netns exec $netns $NS_SHELL \
    || :  # even if shell exists with nonzero, do not fail this script

clear
echo "Exited virtual network"

if [[ $configure == 1 ]]; then
    # cleanup
    ip netns delete $netns
    ip link delete $tap_eth
fi
