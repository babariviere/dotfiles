{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.ssh;
in {
  options.dotfiles.services.ssh.enable = lib.mkEnableOption "ssh";

  config = mkIf cfg.enable { programs.ssh = { startAgent = true; }; };
}
