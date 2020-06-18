#!/usr/bin/env bash

active=$(systemctl --user is-active picom)

if ! [[ $active = "active" ]]; then
	systemctl --user start picom &
else
	systemctl --user stop picom &
fi
