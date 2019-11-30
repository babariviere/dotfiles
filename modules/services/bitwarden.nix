{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.bitwarden;
in {
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ bitwarden-cli ];
  };
}
