zinit ice pick"dracula.zsh-theme" src"lib/async.zsh"
# zinit light dracula/zsh
zinit load %HOME/src/github.com/dracula/zsh

zinit light zsh-users/zsh-history-substring-search
zinit light zdharma/history-search-multi-word

# zinit ice wait'1' lucid
# zinit light changyuheng/fz

# zinit ice src'z.sh'
# zinit light rupa/z

zinit ice wait lucid
zinit light laggardkernel/zsh-thefuck

zinit light Aloxaf/fzf-tab

zinit wait lucid atload"zicompinit; zicdreplay" blockf for \
  ryutok/rust-zsh-completions

zinit wait lucid for \
 atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
    zdharma/fast-syntax-highlighting \
 blockf \
    zsh-users/zsh-completions \
 atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions

zinit ice lucid blockf
zinit light ${ASDF_DIR}/completions

zinit as"null" wait"1" lucid for \
    sbin    Fakerr/git-recall \
    sbin    cloneopts paulirish/git-open \
    sbin    paulirish/git-recent \
    sbin    davidosomething/git-my \
    sbin atload"export _MENU_THEME=legacy" \
            arzzen/git-quick-stats \
    sbin    iwata/git-now \
    make"PREFIX=$ZPFX install" \
            tj/git-extras \
    sbin"git-url;git-guclone" make"GITURL_NO_CGITURL=1" \
            zdharma/git-url

for f in $HOME/.zsh/plugins/*.zsh(N); do
  source $f
done
