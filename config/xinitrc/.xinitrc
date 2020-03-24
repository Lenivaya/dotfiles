#!/bin/sh

[ "$(command -v xrdb)" ] && [ -f "$HOME/.Xresources" ] && \
    xrdb "$HOME/.Xresources"

if [ "$(command -v xset)" ]
then
    xset b off

    xset mouse 0 0

    xset r rate 250 50

    xset dpms force on
    xset s default

    xset +fp ~/.fonts/misc/

    xset fp rehash
fi

[ "$(command -v xsetroot)" ] && \
    xsetroot -cursor_name left_ptr

PAPE="$HOME/Pictures/walls/gregory_crewdson-Brief_Encounter.jpg"
[ "$(command -v hsetroot)" ] && [ -f "$PAPE" ] && \
    hsetroot -cover "$PAPE"

[ "$(command -v xautolock)" ] && \
    xautolock -detectsleep -time 20 -locker "betterlockscreen -l dim" &

[ "$(command -v setxkbmap)" ] && \
    setxkbmap -layout "us, ru, ua" -option "grp:win_space_toggle" -option "compose:menu" -option "caps:ctrl_modifier"

[ "$(command -v synclient)" ] && \
    synclient  TapButton1=1 TapButton2=3 TapButton3=1 VertEdgeScroll=0 HorizEdgeScroll=0 HorizScrollDelta=-111 VertScrollDelta=-111

[ "$(command -v syndaemon)" ] && \
    syndaemon -i 0.5 -t -K -R &

[ "$(command -v clight)" ] && [ -f "$PAPE" ] && \
    clight &

[ "$(command -v redshift)" ] && \
    redshift -c "$HOME/.config/redshift/redshift.conf" &
    disown redshift

[ "$(command -v dunst)" ] && \
    dunst &

[ "$(command -v mpd)" ] && \
    mpd

[ "$(command -v dex)" ] && \
    dex /etc/xdg/autostart/org.kde.kdeconnect.daemon.desktop

[ "$(command -v picom)" ] && picom -b

exec xmonad
