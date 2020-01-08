#!/bin/sh

if mpc | grep -q '\[playing\]'
then
    mpc pause
else
    mpc play
fi
