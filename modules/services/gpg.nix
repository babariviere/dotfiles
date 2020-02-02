{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.gpg;
in {
  options.dotfiles.services.gpg.enable = lib.mkEnableOption "gpg";

  config = mkIf cfg.enable { programs.gnupg.agent.enable = true; };
}
