{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.gpg;
in { config = mkIf cfg.enable { programs.ssh = { startAgent = true; }; }; }