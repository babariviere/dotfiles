{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  config = lib.mkIf cfg.dev.go.enable {
    environment.systemPackages = with pkgs; [ go gotools ];
  };
}
