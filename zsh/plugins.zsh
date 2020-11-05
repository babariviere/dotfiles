zinit ice pick"dracula.zsh-theme" src"lib/async.zsh"
zinit light dracula/zsh

zinit light zsh-users/zsh-history-substring-search
zinit light zdharma/history-search-multi-word

zinit ice wait'1' lucid
zinit light changyuheng/fz

zinit ice src'z.sh'
zinit light rupa/z

zinit ice wait lucid
zinit light laggardkernel/zsh-thefuck

zinit light Aloxaf/fzf-tab

zinit as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
    atpull'%atclone' pick"direnv" src"zhook.zsh" for \
    direnv/direnv

zinit wait lucid atload"zicompinit; zicdreplay" blockf for \
  ryutok/rust-zsh-completions

zinit wait lucid for \
 atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
    zdharma/fast-syntax-highlighting \
 blockf \
    zsh-users/zsh-completions \
 atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions

for f in $HOME/.zsh/plugins/*.zsh(N); do
  source $f
done
