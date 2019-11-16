## History
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
# emacs
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
# vi
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
