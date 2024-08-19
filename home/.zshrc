if command -v neofetch &> /dev/null
then
	neofetch
fi

[ -f "/home/manhtq/.ghcup/env" ] && . "/home/manhtq/.ghcup/env" # ghcup-env
export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="candy"

zstyle ':omz:update' mode reminder
zstyle ':omz:update' frequency 13

ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

ISABLE_UNTRACKED_FILES_DIRTY="true"

HIST_STAMPS="mm/dd/yyyy"

plugins=(git zsh-autosuggestions command-not-found colored-man-pages gh)

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

unalias -m '*' # Remove all zsh alias

source ~/.alias.sh # Load custom alias

eval $(thefuck --alias)
eval "$(zoxide init zsh)"
