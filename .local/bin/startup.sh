#!/bin/bash

# Wallpaper
feh --bg-fill --no-fehbg --randomize ~/.local/share/wallpapers

# Rounded corner
picom &

# Cursor
xsetroot -cursor_name left_ptr

# Polybar
killall -q polybar
polybar &

# Set layout update
killall -q update_layout.py
~/.local/bin/update_layout.py &

# Ibus-daemon
ibus-daemon -rxRd

# Clipboard Manager
greenclip daemon &

# Power manager
xfce4-power-manager &

# Network Manager Applet
if [ -x /usr/bin/nm-applet ]; then
    nm-applet --sm-disable &
fi

# Bluetooth Applet
if [ -x /usr/bin/blueman-applet ]; then
    blueman-applet &
fi
