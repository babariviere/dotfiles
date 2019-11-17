source $ZGEN_SOURCE/zgen.zsh

if ! zgen saved; then
  echo "Initializing zgen"

  zgen load hlissner/zsh-autopair autopair.zsh
  zgen load zsh-users/zsh-history-substring-search
  zgen load zdharma/history-search-multi-word
  zgen load zsh-users/zsh-completions src
  zgen load zsh-users/zsh-syntax-highlighting
  zgen load zsh-users/zsh-autosuggestions

  if [[ -z $SSH_CONNECTION ]]; then
    zgen load zdharma/fast-syntax-highlighting
  fi

  zgen save
fi

source $ZDOTDIR/prompt.zsh
source $ZDOTDIR/config.zsh
source $ZDOTDIR/keybinds.zsh
for file in $ZDOTDIR/rc.d/aliases.*.zsh(N); do
  source $file
done
