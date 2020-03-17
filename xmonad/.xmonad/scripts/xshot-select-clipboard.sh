#!/bin/sh

maim --format=png -s --hidecursor --quality 1 | xclip -selection clipboard -t image/png
