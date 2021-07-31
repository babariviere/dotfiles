{ config, lib, pkgs, ... }:

let
  emacs = pkgs.emacsOsx;
  emacsclient = pkgs.writeScriptBin "emacsclient" ''
    #!${pkgs.runtimeShell}

    if [ -z "$1" ]; then
    exec ${emacs}/bin/emacsclient -c -n -a "" "$@"
    else
    exec ${emacs}/bin/emacsclient -n -a "" "$@"
    fi
  '';
in {
  programs.emacs = {
    enable = true;
    package = emacs;
  };

  programs.zsh = { shellAliases = { e = "${emacsclient}/bin/emacsclient"; }; };

  home.sessionPath = [ "$HOME/.emacs.d/bin" ];

  home.file = {
    ".doom.d" = {
      source = "${config.dotfiles.configDir}/doom.d";
      recursive = true;
      onChange = "${config.home.homeDirectory}/.emacs.d/bin/doom sync";
    };
    # Not working as planned
    # ".emacs.d" = {
    #   source = doom-emacs;
    #   recursive = true;
    #   onChange = "~/.emacs.d/bin/doom upgrade";
    # };
  };

  # FIXME(babariviere): doesn't work on darwin
  # services.emacs = {
  #   enable = true;
  #   package = emacs;
  # };

  home.sessionVariables = {
    EDITOR = "${emacs}/bin/emacsclient -nw";
    VISUAL = "${emacsclient}/bin/emacsclient";
  };
}
