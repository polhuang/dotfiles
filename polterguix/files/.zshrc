# history
HISTFILE=~/.histfile
HISTSIZE=5000
SAVEHIST=$HISTSIZE
setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY

# autocompletion
autoload -Uz compinit
compinit
zstyle ':completion:*' format 'Presenting the compleat and unabridged %d'
zstyle ':completion:*' list-prompt '%S%F{blue} %p: %f%k'
zstyle ':completion:*' use-cache yes
zstyle ':completion:*:default' menu select=1

# emacs bindings
bindkey -e

# env variables
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.bin"
export XDG_DATA_DIRS="$HOME/.local/share:$HOME/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:$XDG_DATA_DIRS"

# on guix systems, go to ~/.config/zsh/ and delete the cache to reconfigure if using a package manager. avoiding here.

TMOUT=600

TRAPALRM() {
    asciiquarium
}

# aliases
alias ll="ls -alh --color=auto"
alias ls="ls -aF --color=auto"
alias config="cd ~/.config/"
alias dotfiles="cd ~/.dotfiles/"
alias downloads="cd ~/Downloads/"
alias documents="cd ~/Documents/"
alias vim="nvim"

# starship
eval "$(starship init zsh)"

