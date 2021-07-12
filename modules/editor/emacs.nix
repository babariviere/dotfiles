{ config, lib, pkgs, ... }:

let
  cfg = config.my.editor.emacs;
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
  options.my.editor.emacs = with lib; { enable = mkEnableOption "emacs"; };

  config = lib.mkIf cfg.enable {
    hm = {
      programs.emacs = {
        enable = true;
        package = emacs;
      };

      programs.zsh = {
        shellAliases = { e = "${emacsclient}/bin/emacsclient"; };
      };
    };

    home.sessionPath = [ "$HOME/.emacs.d/bin" ];

    home.file = {
      ".doom.d" = {
        source = "${config.dotfiles.configDir}/doom.d";
        recursive = true;
        onChange = "${config.user.home}/.emacs.d/bin/doom sync";
      };
      # Not working as planned
      # ".emacs.d" = {
      #   source = doom-emacs;
      #   recursive = true;
      #   onChange = "~/.emacs.d/bin/doom upgrade";
      # };
    };

    services.emacs = {
      enable = true;
      package = emacs;
    };

    env = {
      EDITOR = "${emacs}/bin/emacsclient -nw";
      VISUAL = "${emacsclient}/bin/emacsclient";
    };
  };
}
