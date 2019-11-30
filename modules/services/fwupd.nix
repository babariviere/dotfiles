{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.fwupd;
in { config = mkIf cfg.enable { services.fwupd.enable = true; }; }
