# -*- mode: sh; sh-shell: zsh; -*-

bindkey -e

# prompt
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )

zstyle ':vcs_info:git:*' formats ' %F{240}(%b)%f'
zstyle ':vcs_info:*' enable git

_collapsed_wd() {
  local i pwd
  pwd=("${(s:/:)PWD/#$HOME/~}")
  if (( $#pwd > 1 )); then
    for i in {1..$(($#pwd-1))}; do
      if [[ "$pwd[$i]" = .* ]]; then
        pwd[$i]="${${pwd[$i]}[1,2]}"
      else
        pwd[$i]="${${pwd[$i]}[1]}"
      fi
    done
  fi
  echo "${(j:/:)pwd}"
}

setopt prompt_subst
PROMPT='%F{blue}%n%f@%m %F{blue}$(_collapsed_wd)%f${vcs_info_msg_0_}> '

# direnv
eval "$(direnv hook zsh)"

# autosuggest
ZSH_AUTOSUGGEST_MANUAL_REBIND=true
ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# autocomplete
zstyle ':autocomplete:*' min-delay 0.25
zstyle ':autocomplete:*' min-input 2
zstyle ':autocomplete:*' widget-style menu-complete

# aliases
alias ls='ls --color=auto'
alias cat='bat -pp'
alias gco='git checkout'
alias gs='git status'
