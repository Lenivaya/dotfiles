#!/usr/bin/env bash

if ! pgrep redshift
then
    redshift -c "$XDG_CONFIG_HOME/redshift/redshift.conf" &
    disown redshift
else
    pkill redshift
fi
