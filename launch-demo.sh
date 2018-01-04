#!/usr/bin/env bash
set -e
set -o pipefail

stack build --install-ghc
stack exec demo -- $@
