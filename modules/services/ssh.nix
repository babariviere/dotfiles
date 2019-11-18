{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.ssh;
in { config = mkIf cfg.enable { programs.ssh = { startAgent = true; }; }; }
