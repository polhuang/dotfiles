# history
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=50000
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY

# emacs-style keybindings
bindkey -e

# for compinstall
zstyle :compinstall filename '/home/polhuang/.zshrc'

# znap
[[ -r ~/.zsh/znap/znap.zsh ]] ||
    git clone --depth 1 -- \
        https://github.com/marlonrichert/zsh-snap.git ~/.zsh/znap

source ~/.zsh/znap/znap.zsh  # Start Znap

# znap packages
znap source marlonrichert/zsh-autocomplete
znap source zsh-users/zsh-syntax-highlighting
znap source zsh-users/zsh-autosuggestions
znap source rupa/z

# asciiquarium
TMOUT=600

TRAPALRM() {
    asciiquarium
}

# starship
eval "$(starship init zsh)"

# Generated for envman. Do not edit.
# [ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"
# export PATH=$PATH:/home/polhuang/Downloads/kontroll/target/release
