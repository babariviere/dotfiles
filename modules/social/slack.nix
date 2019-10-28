{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.social.slack;
in {
  config =
    lib.mkIf cfg.enable { environment.systemPackages = with pkgs; [ slack ]; };
}
