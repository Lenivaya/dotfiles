#!/bin/sh

if mpc | grep -q '\[playing\]'
then
    mpc --no-status pause
else
    mpc --no-status play
fi
