{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.tools.insomnia;
  unstable = import <nixpkgs-unstable> { };
in {
  config =
    lib.mkIf cfg.enable { environment.systemPackages = [ unstable.insomnia ]; };
}
