{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.social.discord;
  unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
in {
  config =
    lib.mkIf cfg.enable { environment.systemPackages = [ unstable.discord ]; };
}
