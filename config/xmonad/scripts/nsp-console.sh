#!/bin/sh

if tmux list-sessions | grep -q "scratchpad"
then
    tmux attach-session -t "scratchpad"
else
    tmux new-session -s "scratchpad" -n "scratchpad" -c "$HOME"
fi
