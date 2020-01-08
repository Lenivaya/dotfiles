#!/bin/sh

PIC="$HOME/Pictures/screenshots/$(date +%Y-%m-%d_%H-%M-%S).png"

maim --format=png --hidecursor --quality 10 --select "$PIC"
