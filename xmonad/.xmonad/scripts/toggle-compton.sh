#!/usr/bin/env bash

if ! pgrep compton
then
    compton -b --config "${XDG_CONFIG_HOME}/compton.conf"
else
    pkill compton
fi
