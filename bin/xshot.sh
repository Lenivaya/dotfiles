#!/bin/sh

PIC="$HOME/Pictures/screenshots/$(date +%Y-%m-%d_%H-%M-%S).png"

maim --format=png --hidecursor "$PIC" &
