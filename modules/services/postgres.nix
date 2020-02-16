{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.postgres;
in {
  options.dotfiles.services.postgres.enable = lib.mkEnableOption "postgres";

  config = mkIf cfg.enable {
    services.postgresql = {
      enable = true;
      enableTCPIP = false; # allow only local
      ensureUsers = [{
        name = "postgres";
        ensurePermissions = {
          "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
        };
      }];
      initialScript = pkgs.writeText "init.sql" ''
        ALTER USER postgres WITH PASSWORD 'postgres';
      '';
    };
  };
}
