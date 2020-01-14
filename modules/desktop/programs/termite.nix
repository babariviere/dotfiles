{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.termite;
in {
  options.dotfiles.desktop.termite.enable = lib.mkEnableOption "termite";

  # TODO: if we want support for emacs colors, we need to add this: https://github.com/syl20bnr/spacemacs/wiki/Terminal
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ termite ];
    environment.shellAliases = { ssh = "TERM=xterm ssh"; };

    home-manager.users."${dotfiles.user}" = {
      home.file.".terminfo/t/termite".source = <config/termite/terminfo>;
      xdg.configFile = {
        "termite/config" = {
          source = pkgs.mutate <config/termite/config> dotfiles.colors;
          recursive = true;
        };
      };
    };
  };
}
