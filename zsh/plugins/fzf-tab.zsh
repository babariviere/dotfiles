enable-fzf-tab

if [ -n "$TMUX" ]; then
  # Use popup
  zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
  zstyle ':fzf-tab:*' popup-pad 50 20
fi

local fzf_flags=(
  --color=light
  --color=hl:#373a42
  --color=bg+:#f0f0f0
  --color=hl+:#373a42
)
zstyle ':fzf-tab:*' fzf-flags $fzf_flags

zstyle ":completion:*:git-checkout:*" sort false
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
zstyle ':fzf-tab:complete:exa:*' fzf-preview 'exa -1 --color=always $realpath'
