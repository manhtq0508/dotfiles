#!/bin/bash

# Wallpaper
feh --bg-fill --no-fehbg ~/.config/wallpapers/wallpaper-1.jpg

# Rounded corner
picom &

# Cursor
xsetroot -cursor_name left_ptr

# Polybar
killall -q polybar
polybar &

# Set layout update
killall -q update_layout.py
~/.config/polybar/update_layout.py &

# Ibus-daemon
ibus-daemon -rxRd

# Network Manager Applet
if [ -x /usr/bin/nm-applet ]; then
    nm-applet --sm-disable &
fi

# Bluetooth Applet
if [ -x /usr/bin/blueman-applet ]; then
    blueman-applet &
fi
