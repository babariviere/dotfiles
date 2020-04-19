{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  options.dotfiles.dev.guile.enable = lib.mkEnableOption "guile";

  config = lib.mkIf cfg.dev.guile.enable {
    environment.systemPackages = with pkgs.unstable; [
      guile
      chicken
      scheme48
      chez
      chibi
    ];
  };
}
