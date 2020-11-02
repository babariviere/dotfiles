source $HOME/.zplug/init.zsh

zplug "dracula/zsh", as:theme

zplug "zsh-users/zsh-history-substring-search"
zplug "zdharma/history-search-multi-word"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"

zplug "changyuheng/fz", defer:1
zplug "rupa/z", use:z.sh

zplug "zdharma/fast-syntax-highlighting", if:"[[ -z $SSH_CONNECTION ]]"

zplug "laggardkernel/zsh-thefuck"
zplug "Aloxaf/fzf-tab"

if ! zplug check --verbose; then
  zplug install
fi

zplug load

for f in $HOME/.zsh/plugins/*.zsh(N); do
  source $f
done
