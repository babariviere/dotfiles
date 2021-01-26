disable-fzf-tab

if [ -n "$TMUX" ]; then
  # Use popup
  zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
  zstyle ':fzf-tab:*' popup-pad 50 20
fi

zstyle ":fzf-tab:*" default-color $'\033[33;5;37m'
FZF_TAB_GROUP_COLORS=()
for i in {1..16} ; do
    # FZF_TAB_GROUP_COLORS+="$(colorfg 10 255 10)"
    FZF_TAB_GROUP_COLORS+=($'\033[38;5;19m')
done
zstyle ':fzf-tab:*' group-colors $FZF_TAB_GROUP_COLORS

zstyle ":completion:*:git-checkout:*" sort false
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
zstyle ':fzf-tab:complete:exa:*' fzf-preview 'exa -1 --color=always $realpath'
