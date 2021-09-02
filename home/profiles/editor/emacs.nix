{ config, lib, pkgs, ... }:

let emacs = pkgs.amber-emacs;
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
  home.packages = [ emacs ]
    ++ (lib.optionals pkgs.stdenv.isDarwin [ pkgs.emacs-client ]);
  programs.emacs.package = emacs;
  programs.zsh = { shellAliases = { e = "${emacs}/bin/emacsclient"; }; };

  env = {
    EDITOR = "${emacs}/bin/emacsclient -nw";
    VISUAL = "${emacs}/bin/emacsclient";
  };
}
