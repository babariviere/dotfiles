{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.social.signal;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ signal-desktop ];
  };
}
