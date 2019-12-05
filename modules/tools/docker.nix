{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.tools.docker;
in {
  options.dotfiles.tools.docker = {
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
      (lib.mkIf cfg.arion (import (builtins.fetchTarball
        "https://github.com/hercules-ci/arion/tarball/master") { }).arion)
    ];

    users.users."${dotfiles.user}".extraGroups = [ "docker" ];
  };
}
