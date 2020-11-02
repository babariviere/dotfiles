source $HOME/.zplug/init.zsh

zplug "dracula/zsh", as:theme

zplug "zsh-users/zsh-history-substring-search"
zplug "zdharma/history-search-multi-word"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"

zplug "changyuheng/fz", defer:1
zplug "rupa/z", use:z.sh

zplug "laggardkernel/zsh-thefuck"
zplug "Aloxaf/fzf-tab"

zplug load

for f in $HOME/.zsh/plugins/*.zsh(N); do
  source $f
done
