#
# ~/.bashrc
#

function check_command() {
    if command -v "$1" &> /dev/null; then
        eval "$2"
    else
        echo -e "\e[33m[ WARN ] $1 is not installed!\e[0m"
    fi
}

check_command "neofetch" "neofetch"

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env
export PATH="$PATH:/usr/local/bin:/usr/local/sbin:/opt/bin:$HOME/.local/bin:$HOME/bin:$HOME/.cargo/bin"

# Setup dark theme
export GTK_THEME=Adwaita:dark
export GTK2_RC_FILES=/usr/share/themes/Adwaita-dark/gtk-2.0/gtkrc
export QT_STYLE_OVERRIDE=Adwaita-Dark

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source ~/.alias.sh
PS1='[\u@\h \W]\$ '

check_command "thefuck" 'eval "$(thefuck --alias)"'
check_command "zoxide" 'eval "$(zoxide init bash)"'
