{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.termite;
in {
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ termite ];

    home-manager.users."${dotfiles.user}".xdg.configFile = {
      "termite" = {
        source = <config/termite>;
        recursive = true;
      };
    };
  };
}
