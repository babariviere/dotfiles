{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  options.dotfiles.dev.latex.enable = lib.mkEnableOption "latex";

  config = lib.mkIf cfg.dev.latex.enable {
    environment.systemPackages = with pkgs; [
      texlive.combined.scheme-full
      zathura
    ];
  };
}
