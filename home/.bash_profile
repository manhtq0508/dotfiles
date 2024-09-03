[ -f "$HOME/.env" ] && . "$HOME/.env"

if [[ "$(tty)" = "/dev/tty1" ]]; then
	pgrep xmonad || startx
fi
