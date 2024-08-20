#!/bin/sh

userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"
sysresources="/etc/X11/xinit/.Xresources"
sysmodmap="/etc/X11/xinit/.Xmodmap"

# Merge in defaults and keymaps

if [ -f "$sysresources" ]; then
    xrdb -merge "$sysresources"
fi

if [ -f "$sysmodmap" ]; then
    xmodmap "$sysmodmap"
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# Start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Wallpaper
feh --bg-fill --nofehbg ~/.config/wallpapers/wallpaper-1.jpg

# Rounded corner
picom &

# Cursor
xsetroot -cursor_name left_ptr

# Polybar
polybar &

# Input method
ibus-daemon &

# Network Manager Applet
if [ -x /usr/bin/nm-applet ]; then
    nm-applet --sm-disable &
fi

# Bluetooth Applet
if [ -x /usr/bin/blueman-applet ]; then
    blueman-applet &
fi

# Start XMonad
exec xmonad
