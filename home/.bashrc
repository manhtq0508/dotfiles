function check_command() {
    if command -v "$1" &> /dev/null; then
        eval "$2"
    else
        echo -e "\e[33m[ WARN ] $1 is not installed!\e[0m"
    fi
}

check_command "neofetch" "neofetch"

# Load $PATH, setting dark theme
[ -f "$HOME/.env" ] && . "$HOME/.env"
# Load alias
source ~/.alias.sh

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

check_command "thefuck" 'eval "$(thefuck --alias)"'
check_command "zoxide" 'eval "$(zoxide init bash)"'
