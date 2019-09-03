#!/bin/bash

printf '\e[8;50;150t'

session=Controllers
window=${session}:0
N=${1:-3}
proposal_delay=${2:-3s}

start_time=$(($(date +%s%N -d '1 second') / 1000))

controller_options="-n $N --batch-size 1 --protocol-start-port 4020 --proposals-delay $proposal_delay --start-time ${start_time}µs"

tmux new-session -d -s "$session"

for (( i = 1; i < $N; i++ )); do
    tmux split-window -v
    tmux select-layout tiled
done

for (( i = 0; i < $N; i++ )); do
    tmux select-pane -t $i
    no=$(($i + 1))
    tmux send-keys "echo \"Running controller #$no\"" C-m
    tmux send-keys "stack exec bench-distr -- -i $no $controller_options" C-m
done

tmux select-pane -t 0

tmux attach-session -t "$session"
