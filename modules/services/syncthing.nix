{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.services.dotfiles;
  cfg = dotfiles.services.syncthing;
in {
  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      user = "${dotfiles.user}";
      configDir = "/home/${dotfiles.user}/.config/syncthing";
      dataDir = "/home/${dotfiles.user}/sync";
    };
  };
}
