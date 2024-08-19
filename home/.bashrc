#
# ~/.bashrc
#

if command -v neofetch &> /dev/null 
then
	neofetch
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source ~/.alias.sh
PS1='[\u@\h \W]\$ '

eval "$(thefuck --alias)"
eval "$(zoxide init bash)"

[ -f "/home/manhtq/.ghcup/env" ] && . "/home/manhtq/.ghcup/env" # ghcup-env

export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH

# Setup dark theme
export GTK_THEME=Adwaita:dark
export GTK2_RC_FILES=/usr/share/themes/Adwaita-dark/gtk-2.0/gtkrc
export QT_STYLE_OVERRIDE=Adwaita-Dark. 
