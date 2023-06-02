#!/bin/sh

maim --format=png -s --hidecursor | xclip -selection clipboard -t image/png
