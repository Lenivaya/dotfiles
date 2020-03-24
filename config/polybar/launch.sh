#!/usr/bin/env sh

killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar -c ~/.config/polybar/bspwm/config top &
# notify-send "Bars launched..."
