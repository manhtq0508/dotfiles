#!/bin/bash

# Wallpaper
feh --bg-fill --no-fehbg --randomize ~/.local/share/wallpapers

# Rounded corner
picom &

# Cursor
xsetroot -cursor_name left_ptr

# Polybar
killall -q polybar
polybar -c ~/.config/polybar/top_bar.ini &
polybar -c ~/.config/polybar/bottom_bar.ini &

# Set layout update
killall -q update_layout.py
~/.config/polybar/scripts/update_layout.py &

# Ibus-daemon
ibus-daemon -rxRd

# Clipboard Manager
greenclip daemon &

# Power manager
xfce4-power-manager &

# Night light
killall -q redshift redshift-gtk
redshift &

# Keyring
gnome-keyring-daemon &

# Network Manager Applet
if [ -x /usr/bin/nm-applet ]; then
    nm-applet --sm-disable &
fi

# Bluetooth Applet
if [ -x /usr/bin/blueman-applet ]; then
    blueman-applet &
fi
