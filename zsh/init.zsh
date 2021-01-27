source $HOME/.profile

# TODO: move env to zshprofile ?
export KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --with-wx"
export EDITOR="nvim"
export ECTO_EDITOR="$EDITOR"
# Fix issue on MacOS
export GPG_TTY=$(tty)
export MANPAGER='nvim +Man!'
export MANWIDTH=999

export BAT_THEME="OneHalfLight"
export FZF_DEFAULT_OPTS='--color=light'

# Go setup
export GOBIN="$HOME/.local/bin"
export GOCACHE="$HOME/.cache/go"
export GOMODCACHE="$HOME/.cache/gomod"
# export GOPATH="$HOME"
export GO111MODULE=on

ulimit -Sn 20000

# Use vi bindings
set -o vi

# Setup h
# export PATH=$HOME/src/github.com/zimbatm/h:$PATH
# eval "$(h --setup $HOME/src)"

# Setup flow
eval "$(flow setup $HOME/src)"

# Setup direnv
eval "$(direnv hook zsh)"

# Setup rust
if [ -f "$HOME/.cargo/env" ]; then
  source "$HOME/.cargo/env"
fi

# Add .bin if it exists
if [ -d "$HOME/.bin" ]; then
  export PATH="$HOME/.bin:$PATH"
fi
export PATH="$HOME/.local/bin:$PATH"

# Add nix
if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
  source "$HOME/.nix-profile/etc/profile.d/nix.sh"

  # Home manager
  if [ -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
    export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
    source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
  fi
fi

# Dracula config
# export DRACULA_DISPLAY_CONTEXT=1

# source $HOME/.zsh/prompt.zsh
source $HOME/.zsh/config.zsh
source $HOME/.zsh/aliases.zsh
source $HOME/.zsh/functions.zsh
source $HOME/.zsh/plugins.zsh
source $HOME/.zsh/keybinds.zsh

for file in $HOME/.zsh/rc.d/aliases.*.zsh(N); do
  source $file
done

for file in $HOME/.zsh/rc.d/env.*.zsh(N); do
  source $file
done

_set_block_cursor() { echo -ne "\x1b[\x32 q" }
_set_beam_cursor() { echo -ne "\x1b[\x36 q" }

# vi cursor
zle-keymap-select() {
  case $KEYMAP in
    vicmd) _set_block_cursor;; # block cursor
    viins|main) _set_beam_cursor;; # beam cursor
  esac
}

zle -N zle-keymap-select

zle-line-init() { zle -K viins; _set_beam_cursor }
zle-line-finish() { _set_block_cursor }
zle -N zle-line-init
zle -N zle-line-finish
