{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.social.signal;
in {
  options.dotfiles.social.signal.enable = lib.mkEnableOption "signal";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ signal-desktop ];
  };
}
