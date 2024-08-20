function check_command() {
    if command -v "$1" &> /dev/null; then
        eval "$2"
    else
        echo -e "\e[33m[ WARN ] $1 is not installed\e[0m"
    fi
}

check_command "neofetch" "neofetch"

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env
export PATH="$PATH:/usr/local/bin:/usr/local/sbin:/opt/bin:$HOME/.local/bin:$HOME/bin"

# Setup dark theme
export GTK_THEME=Adwaita:dark
export GTK2_RC_FILES=/usr/share/themes/Adwaita-dark/gtk-2.0/gtkrc
export QT_STYLE_OVERRIDE=Adwaita-Dark. 

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="candy"

zstyle ':omz:update' mode reminder
zstyle ':omz:update' frequency 13

ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

ISABLE_UNTRACKED_FILES_DIRTY="true"

HIST_STAMPS="mm/dd/yyyy"

plugins=(
	git
	zsh-autosuggestions
	command-not-found
	colored-man-pages
	gh
)

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

unalias -m '*' # Remove all zsh alias
source ~/.alias.sh # Load custom alias


check_command "thefuck" 'eval "$(thefuck --alias)"'
check_command "zoxide" 'eval "$(zoxide init bash)"'
