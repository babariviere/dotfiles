use_asdf() {
    if [ ! -d "$HOME/.asdf" ]; then
        return
    fi

    export ASDF_DIRENV_VERSION="system"
    source ~/.asdf/asdf.sh

    # Setup asdf
    if [ ! -d "$ASDF_DIR/plugins/direnv" ]; then
        asdf plugin add direnv
    fi

    source_env "$(asdf direnv envrc "$@")"
}
