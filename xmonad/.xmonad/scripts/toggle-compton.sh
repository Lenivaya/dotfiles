#!/usr/bin/env bash

if ! pgrep compton
then
    compton -b 
else
    pkill compton
fi
