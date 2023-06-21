#!/usr/bin/env bash

if ! systemctl --user is-active --quiet picom; then
  systemctl --user start picom &
else
  systemctl --user stop picom &
fi
