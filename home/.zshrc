function check_command() {
    if command -v "$1" &> /dev/null; then
        eval "$2"
    else
        echo -e "\e[33m[ WARN ] $1 is not installed\e[0m"
    fi
}

check_command "neofetch" "neofetch"

# Load $PATH, setting dark theme
[ -f "$HOME/.env" ] && . "$HOME/.env"

# Oh My Zsh configuration
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"

zstyle ':omz:update' mode reminder
zstyle ':omz:update' frequency 13

ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="dd/mm/yyyy"

plugins=(
    git
    zsh-autosuggestions
    command-not-found
    colored-man-pages
    gh
    zsh-syntax-highlighting
)

source "$ZSH/oh-my-zsh.sh"

# Set language
export LANG=en_US.UTF-8

# Unalias all zsh aliases and load custom aliases
unalias -m '*' # Remove all zsh alias
source ~/.alias.sh # Load custom alias

# Check and configure additional commands
check_command "thefuck" 'eval "$(thefuck --alias)"'
check_command "zoxide" 'eval "$(zoxide init zsh)"'

