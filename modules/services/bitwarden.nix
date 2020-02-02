{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.bitwarden;
in {
  options.dotfiles.services.bitwarden.enable = lib.mkEnableOption "bitwarden";

  config =
    mkIf cfg.enable { environment.systemPackages = with pkgs; [ bitwarden ]; };
}
