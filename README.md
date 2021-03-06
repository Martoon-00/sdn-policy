# SDN policies concurrent composition

[![Build Status](https://travis-ci.org/Martoon-00/sdn-policy.svg?branch=master)](https://travis-ci.org/Martoon-00/sdn-policy)

## Overview

This project is prototype for algorithm of concurrent network policies composition.
Keypoints:

* Policies composition and conflicts resolution is assumed to be performed on
control layer, thus _processes_ use algorithm of _consesus_ in order to build 
resulting network configuration.

* Comparing to classical consensus algorithms like _Paxos_, _Raft_, presented
algorithm is optimized for the case when policies do not conflict with each
other and thus _commute_, i.e order of their application is irrelevant.

* Protocol is based on principle of Software Transactional Network, where in
presence of conflict policy is denied and client is notified about that.
This allows easer management over network and provides more control
comparing to protocols which perform some conflict resolution in place.

## Protocol

Most intersting modules are:

* [Classic](./src/Sdn/Protocol/Classic/)
* [Fast](./src/Sdn/Protocol/Fast/)

- [x] Classic Generalized Paxos
- [x] Fast Generalized Paxos (where profit from commuting policies comes into play)

## Launch

To build this project you need [stack](https://docs.haskellstack.org/en/stable/README/) tool.

Executing `cd demo; ./demo/launch-demo.sh` compiles the project and performs 1 consensus ballot with 1 proposed policy.

More complex network topology parameters can be specified in `./topology.yaml` file inside `demo` directory.
Custom config file can be passed via `--config` option.

## Tests

### Properties

Tests consist of several scenarious run and checked on various properties.

* [Protocol](./test/Test/Sdn/Overall/Properties/Protocol.hs) module - contains list of ever checked properties
* [CommonSpec](./test/Test/Sdn/Overall/CommonSpec.hs) module - contains test scenarious common for classic and fast versions of algorithm.
* [FastSpec](./test/Test/Sdn/Overall/FastSpec.hs) module - contains test scenarious special for fast version.

Basic property examples:

* > Only 2 / 3 acceptors are available => learner still learns proposed values

* > Only 1 / 3 acceptor is available => learner doesn't learn proposed values
* > Network delays may exceed ballot duration => learner still learns proposed values

* > All proposed policies conflict, at least one policy was proposed => exactly one policy
  > is learned

Complex real-life properties are present as well.

### Flexibility of scenarious testing

Due to usage of some tools, scenarious written in tests are:

* **emulated** - scenarious are written basing on **time-warp** library, which allows
to run tests instantly regardless of protocol timings, enabling control over network delays;

* **generated** - when scenario allows parametrization, specified numbers are
generated by QuickCheck; among parameters:
  
  * ballots number / duration

  * policy proposal rate
  
  * network delays / nodes availability

* **deterministic** - if test has failed, HSpec will prompt a random seed;
this seed may be used to repeat the test with exactly the same outcome.

Mentioned tools have provided a background for extensive testing and debugging.
