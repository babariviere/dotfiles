{ config, lib, pkgs, ... }:

let
  emacs = pkgs.amber-emacs;
  emacsclient = pkgs.writeScriptBin "emacsclient" ''
    #!${pkgs.runtimeShell}

    if [ -z "$1" ]; then
    exec ${emacs}/bin/emacsclient -c -n -a "" "$@"
    else
    exec ${emacs}/bin/emacsclient -n -a "" "$@"
    fi
  '';
in {
  assertions = [{
    assertion = !config.profiles.editor.doom.enable;
    message = "profiles.editor.doom cannot be enable with emacs.";
  }];

  home.file = {
    ".emacs.d" = {
      source = "${config.dotfiles.configDir}/emacs.d";
      recursive = true;
    };
  };
  home.packages = [ emacs ];
  programs.zsh = { shellAliases = { e = "${emacsclient}/bin/emacsclient"; }; };

  home.sessionVariables = {
    EDITOR = "${emacs}/bin/emacsclient -nw";
    VISUAL = "${emacsclient}/bin/emacsclient";
  };
}
