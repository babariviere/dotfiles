enable-fzf-tab

if [ -n "$TMUX" ]; then
  # Use popup
  zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
  zstyle ':fzf-tab:*' popup-pad 50 20
fi

# FZF_TAB_GROUP_COLORS=(
#     $'\033[94m' $'\033[32m' $'\033[33m' $'\033[35m' $'\033[31m' $'\033[38;5;27m' $'\033[36m' \
#     $'\033[38;5;100m' $'\033[38;5;98m' $'\033[91m' $'\033[38;5;80m' $'\033[92m' \
#     $'\033[38;5;214m' $'\033[38;5;165m' $'\033[38;5;124m' $'\033[38;5;120m'
# )
# zstyle ':fzf-tab:*' group-colors $FZF_TAB_GROUP_COLORS
# local fzf_flags=(
#   --color=hl:#373a42
#   --color=bg+:#f0f0f0
#   --color=hl+:#373a42
# )
# zstyle ':fzf-tab:*' fzf-flags $fzf_flags
# zstyle ':fzf-tab:*' default-color $'\033[30m'

zstyle ":completion:*:git-checkout:*" sort false
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
zstyle ':fzf-tab:complete:exa:*' fzf-preview 'exa -1 --color=always $realpath'
