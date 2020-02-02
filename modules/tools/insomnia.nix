{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.tools.insomnia;
  unstable = import <nixpkgs-unstable> { };
in {
  options.dotfiles.tools.insomnia.enable = lib.mkEnableOption "insomnia";

  config =
    lib.mkIf cfg.enable { environment.systemPackages = [ unstable.insomnia ]; };
}
