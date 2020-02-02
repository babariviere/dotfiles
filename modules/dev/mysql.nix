{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.mysql;
in {
  options.dotfiles.dev.mysql.enable = lib.mkEnableOption "mysql";

  config = lib.mkIf cfg.enable {
    services.mysql = {
      enable = true;
      package = pkgs.mariadb;
    };
  };
}
