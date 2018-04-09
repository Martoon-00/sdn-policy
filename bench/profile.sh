#!/usr/bin/env bash
set -e
set -o pipefail

# This script runs time profiling.
# Memory profiling is available via passing `--alloc` to ghc-prof-flamegraph.

executable="bench"
report_name="report-profiling"

if [[ $1 == "" ]]; then
    flamegraph_width="3000"
else
    flamegraph_width=$1
fi

# note: this line will do nothing if project is already built without profiling
stack build --profile

echo "Running controller"
stack exec $executable -- +RTS -p
echo "Done, gathering report"

ghc-prof-flamegraph $executable.prof --flamegraph-option "--width=$flamegraph_width" -o $report_name.svg
xdg-open $report_name.svg
