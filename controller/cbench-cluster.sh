#!/bin/bash

# This script is used to run several "cbench" instances
# for measuring latency.
# Multiple instances are needed to make several conflicting policies.

printf '\e[8;30;100t'

session=cbenchs
window=${session}:0
N=${1:-3}
cbench_args=$2

tmux new-session -d -s "$session"

for (( i = 1; i < $N; i++ )); do
  tmux split-window -v
  tmux select-layout tiled
done

for (( i = 0; i < $N; i++ )); do
  tmux select-pane -t $i
  no=$(($i + 1))
  target_controller_port=$((6633 + $i))
  tmux send-keys "echo \"Running cbench instance #$no\"" C-m
  tmux send-keys "cbench --port $target_controller_port $cbench_args" C-m
done

tmux select-pane -t 0

tmux attach-session -t "$session"
