#!/bin/sh

STATUS="$(fcitx-remote)"

if   [ "$STATUS" = 0 ]
then
    OUT="Off"
elif [ "$STATUS" = 1 ]
then
    OUT="Disabled"
else
    OUT="Enabled"
fi

echo "$OUT"
