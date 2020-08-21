{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.tools.docker;
in {

  options.dotfiles.tools.docker = {
    enable = lib.mkEnableOption "docker";
    arion = lib.mkEnableOption "arion";
    compose = lib.mkEnableOption "docker-compose";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = true;
    };

    environment.systemPackages = with pkgs; [
      (lib.mkIf cfg.compose docker-compose)
      (lib.mkIf cfg.arion arion)
    ];

    networking.hosts = { "172.17.0.1" = [ "host.docker.internal" ]; };

    users.users."${dotfiles.user}".extraGroups = [ "docker" ];
  };
}
