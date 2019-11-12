{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.tools.sql;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ dbeaver ];
  };
}
