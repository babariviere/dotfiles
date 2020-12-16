## History
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
# emacs
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
# vi
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

## Ctrl+Left and Ctrl+Right
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

bindkey "\e\e" fuck-command-line

# set delete key
bindkey "^?" backward-delete-char

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^e" edit-command-line
bindkey -M vicmd v edit-command-line

bindkey -M vicmd ? history-search-multi-word
bindkey -M vicmd / history-search-multi-word
