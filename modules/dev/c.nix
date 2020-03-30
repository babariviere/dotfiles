{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.c;
in {
  options.dotfiles.dev.c.enable = lib.mkEnableOption "c";

  config =
    lib.mkIf cfg.enable { environment.systemPackages = with pkgs; [ gcc ]; };
}
