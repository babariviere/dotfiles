direnv hook fish | source

if status --is-interactive
    keychain --eval --quiet -Q --agents ssh id_ed25519 | source
    keychain --eval --quiet -Q --agents gpg | source
end
