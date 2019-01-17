#!/bin/bash

printf '\e[8;50;150t'

session=Controllers
window=${session}:0
N=${1:-3}

tmux new-session -d -s "$session"

for (( i = 1; i < $N; i++ )); do
  tmux split-window -v
  tmux select-layout tiled
done

for (( i = 0; i < $N; i++ )); do
  tmux select-pane -t $i
  no=$(($i + 1))
  tmux send-keys "echo \"Running controller #$no\"" C-m
  tmux send-keys "stack exec controller -- -n $N -i $no --controllers-start-port 6632 --protocol-start-port 4020" C-m
done

tmux select-pane -t 0

tmux attach-session -t "$session"
