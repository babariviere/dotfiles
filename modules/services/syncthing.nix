{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.syncthing;
in {
  options.dotfiles.services.syncthing.enable = lib.mkEnableOption "syncthing";

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
