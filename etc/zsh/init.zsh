# -*- mode: sh; sh-shell: zsh; -*-

bindkey -e
eval $(env TERM=xterm256-color dircolors)

# direnv
eval "$(direnv hook zsh)"

# autosuggest
ZSH_AUTOSUGGEST_MANUAL_REBIND=true
ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# aliases
alias ls='ls --color=auto'
alias cat='bat -pp'
alias gco='git checkout'
alias gs='git status'

# z
ZSHZ_CASE=ignore

export PATH="$HOME/.local/bin:$PATH"
