{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.social.riot;
in {
  options.dotfiles.social.riot.enable = lib.mkEnableOption "riot";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ riot-desktop ];
  };
}
