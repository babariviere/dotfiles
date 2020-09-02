if [[ $(uname) == "Darwin" ]]; then
  alias ls="ls -G"
else
  alias ls="ls --color=auto"
fi
alias ll="ls -al"

alias v="nvim \$(fzf)"
alias vim="nvim"
