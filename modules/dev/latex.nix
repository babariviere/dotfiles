{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  config = lib.mkIf cfg.dev.latex.enable {
    environment.systemPackages = with pkgs; [
      texlive.combined.scheme-full
      zathura
    ];
  };
}
