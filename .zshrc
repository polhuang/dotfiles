# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/polhuang/.zshrc'

autoload -Uz compinit
compinit .zcompdump
# End of lines added by compinstall

eval "$(starship init zsh)"

# Download Znap, if it's not there yet.
[[ -r ~/Repos/znap/znap.zsh ]] ||
    git clone --depth 1 -- \
        https://github.com/marlonrichert/zsh-snap.git ~/Repos/znap

source ~/Repos/znap/znap.zsh  # Start Znap

# znap source marlonrichert/zsh-autocomplete

TMOUT=600

TRAPALRM() {
    asciiquarium
}

export PATH="$PATH:/home/polhuang/.local/bin"
export PATH="$PATH:/home/polhuang/.bin"

if (( $+commands[luarocks] )); then
    eval `luarocks path --bin`
fi

source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-autocomplete/zsh-autocomplete.plugin.zshz
