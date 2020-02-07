{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.tools.sql;
in {
  options.dotfiles.tools.sql.enable = lib.mkEnableOption "sql";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      dbeaver
      mysql-client
      python3Packages.sqlparse
    ];
  };
}
