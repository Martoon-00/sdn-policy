#!/usr/bin/env bash
set -e
set -o pipefail

report_name="report-profiling"

# note: this line will do nothing if project is already built without profiling
stack build --profile

echo "Running controller"
stack exec controller -- +RTS -p -hc
echo "Done, gathering report"

mv controller.hp $report_name.hp
hp2pretty $report_name.hp
xdg-open $report_name.svg
