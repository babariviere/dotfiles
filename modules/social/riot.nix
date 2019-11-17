{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.social.riot;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ riot-desktop ];
  };
}
