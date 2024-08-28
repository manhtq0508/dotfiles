#!/bin/bash

# Wallpaper
feh --bg-fill --no-fehbg ~/.local/share/wallpapers/wallpaper-1.jpg

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

# Network Manager Applet
if [ -x /usr/bin/nm-applet ]; then
    nm-applet --sm-disable &
fi

# Power alert
poweralertd &

# Bluetooth Applet
if [ -x /usr/bin/blueman-applet ]; then
    blueman-applet &
fi
