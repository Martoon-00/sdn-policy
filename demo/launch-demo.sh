#!/usr/bin/env bash
set -e
set -o pipefail

# Build and install demo.

stack build --install-ghc
stack exec demo -- $@
