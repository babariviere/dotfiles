# TODO: move env to zshprofile ?
export KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --with-wx"
export EDITOR="nvim"

ulimit -Sn 20000

# Setup h
export PATH=$HOME/src/github.com/zimbatm/h:$PATH
eval "$(h --setup $HOME/src)"
eval "$(up --setup)"

# Setup direnv
eval "$(direnv hook zsh)"

# Add asdf
source $HOME/.asdf/asdf.sh
fpath=(${ASDF_DIR}/completions $fpath)

# Setup rust
if [ -f "$HOME/.cargo/env" ]; then
  source "$HOME/.cargo/env"
fi

# Add .bin if it exists
if [ -d "$HOME/.bin" ]; then
  export PATH="$HOME/.bin:$PATH"
fi

source $ZDOTDIR/prompt.zsh
source $ZDOTDIR/config.zsh
source $ZDOTDIR/keybinds.zsh
source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/functions.zsh
source $ZDOTDIR/plugins.zsh

for file in $ZDOTDIR/rc.d/aliases.*.zsh(N); do
  source $file
done

for file in $ZDOTDIR/rc.d/env.*.zsh(N); do
  source $file
done
