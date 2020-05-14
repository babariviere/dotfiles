{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.python;
in {
  options.dotfiles.dev.python.enable = lib.mkEnableOption "python";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (python3.withPackages
        (ps: with ps; [ setuptools python-language-server pip ]))
      pipreqs
      pypi2nix
    ];
  };
}
