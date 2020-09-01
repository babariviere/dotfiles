source $HOME/.zplug/init.zsh

zplug "zsh-users/zsh-history-substring-search"
zplug "zdharma/history-search-multi-word"
zplug "zsh-users/zsh-completions", dir:src
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"

zplug "changyuheng/fz", defer:1
zplug "rupa/z", use:z.sh

zplug "zdharma/fast-syntax-highlighting", if:"[[ -z $SSH_CONNECTION ]]"

if ! zplug check --verbose; then
  zplug install
fi

zplug load --verbose
