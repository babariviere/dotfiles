{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.python;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      python3
      python3Packages.python-language-server
    ];
  };
}
