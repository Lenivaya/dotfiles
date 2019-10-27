#!/bin/sh

if [ "$(command -v xmodmap)" ] && [ -f "$HOME/.xmodmaprc" ]
then
    xmodmap "$HOME/.xmodmaprc"
fi

[ "$(command -v xrdb)" ] && [ -f "$HOME/.Xresources" ] && \
    xrdb "$HOME/.Xresources"

[ "$(command -v setxkbmap)" ] && \
    setxkbmap -layout "us, ru, ua" -option "grp:win_space_toggle" -option "compose:menu"

[ "$(command -v compton)" ] && compton -b

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

[ "$(command -v synclient)" ] && \
    synclient  TapButton1=1 TapButton2=3 TapButton3=1 VertEdgeScroll=1 HorizScrollDelta=-111 VertScrollDelta=-111

[ "$(command -v syndaemon)" ] && \
    syndaemon -i 0.5 -t -K -R &

[ "$(command -v xautolock)" ] && \
    xautolock -detectsleep -time 20 -locker "betterlockscreen -l dim" &

PAPE=$HOME/Pictures/walls/Johan_Christian_Claussen_Dahl_-_Frederiksborg_slott_\(1817\).jpg
[ "$(command -v hsetroot)" ] && \
    hsetroot -cover $PAPE

[ "$(command -v betterlockscreen)" ] && \
    betterlockscreen -u $PAPE

[ "$(command -v clight)" ] && \
    clight &

[ "$(command -v dunst)" ] && \
    dunst &

[ "$(command -v mpd)" ] && \
    mpd

[ "$(command -v dex)" ] && \
    dex /etc/xdg/autostart/org.kde.kdeconnect.daemon.desktop

exec xmonad
