# TODO: move env to zshprofile ?
export KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --with-wx"

# Setup h
export PATH=~/src/github.com/zimbatm/h:$PATH
eval "$(h --setup ~/src)"
eval "$(up --setup)"

# Setup direnv
eval "$(direnv hook zsh)"

# Add asdf
source $HOME/.asdf/asdf.sh
fpath=(${ASDF_DIR}/completions $fpath)

source $ZDOTDIR/prompt.zsh
source $ZDOTDIR/config.zsh
source $ZDOTDIR/keybinds.zsh
source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/plugins.zsh

for file in $ZDOTDIR/rc.d/aliases.*.zsh(N); do
  source $file
done

for file in $ZDOTDIR/rc.d/env.*.zsh(N); do
  source $file
done
