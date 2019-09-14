{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.dotfiles;
in
{
  require = [
    ../.
  ];
  
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user =  "${cfg.user}";
    configDir = "/home/${cfg.user}/.config/syncthing";
    dataDir = "/home/${cfg.user}/sync";
  };
}
