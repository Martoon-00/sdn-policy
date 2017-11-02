# SDN policies concurrent composition

## Overview

This project is prototype for algorithm of concurrent policies composition.
Keypoints:

* Algorithm is optimized for the case when policies do not conflict with each
other and thus _commute_, i.e order of their application is irrelevant.

* Protocol is based on principle of Software Transactional Network, where in
presence of conflict policy is denied and client is notified about that.
This allows easer network configuration management and provides more control
comparing to protocols which perform some conflict resolution in place.

## Structure

Most intersting modules are:

* **src/Sdn.Protocol.Phases** - logic which lies beyond each phrase
* **src/Sdn.Protocol.Topology** - functions construct a network with appropriate processes

## Tools

This prototype uses **time-warp** library. This allows to write some properties
for protocol, like
> *2/3 acceptors are online => proposed policies still can be applied*

emulating passage of time and network unreliability.
