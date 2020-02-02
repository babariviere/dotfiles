{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.social.slack;
in {
  options.dotfiles.social.slack.enable = lib.mkEnableOption "slack";

  config =
    lib.mkIf cfg.enable { environment.systemPackages = with pkgs; [ slack ]; };
}
