#!/usr/bin/env bash
set -e
set -o pipefail

report_svg_name="report-profiling.svg"

if [[ $1 == "" ]]; then
    flamegraph_width="3000"
else
    flamegraph_width=$1
fi

# note: this line will do nothing if project is already built without profiling
stack build --profile

echo "Running controller"
stack exec controller -- +RTS -p
echo "Done, gathering report"

ghc-prof-flamegraph controller.prof --flamegraph-option "--width=$flamegraph_width" -o $report_svg_name
xdg-open $report_svg_name
