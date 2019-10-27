{ config, lib, pkgs, ... }:

let cfg = config.services.dotfiles;
in {
  config = lib.mkIf cfg.dev.go.enable {
    environment.systemPackages = with pkgs; [ go gotools ];
  };
}
