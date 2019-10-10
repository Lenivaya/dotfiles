#!/bin/sh

ID=$(xinput list | grep -Eo 'TrackPoint\s*id\=[0-9]{1,2}' | grep -Eo '[0-9]{1,2}')
STATE=$(xinput list-props "$ID" | awk '/Device Enabled/ {print $4}')

if [ "$STATE" -eq 1 ]
then
    xinput disable "$ID"
else
    xinput enable "$ID"
fi
