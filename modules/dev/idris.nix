{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  options.dotfiles.dev.idris.enable = lib.mkEnableOption "idris";

  config = lib.mkIf cfg.dev.idris.enable {
    environment.systemPackages = lib.singleton pkgs.unstable.idris2;
  };
}
