{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.social.discord;
in {
  options.dotfiles.social.discord.enable = lib.mkEnableOption "discord";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.unstable.discord ];
  };
}
