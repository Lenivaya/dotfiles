#!/bin/sh

session_name="scratchpad"

if tmux has-session -t "$session_name"; then
  tmux attach-session -t "scratchpad"
else
  tmux new-session -s "$session_name" -n "$session_name" -c "$HOME"
fi
