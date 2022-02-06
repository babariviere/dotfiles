direnv hook fish | source

if status --is-interactive
    keychain --eval --quiet -Q --agents ssh,gpg id_ed25519 | source
end
