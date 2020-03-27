{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.tools.insomnia;
in {
  options.dotfiles.tools.insomnia.enable = lib.mkEnableOption "insomnia";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.unstable.insomnia ];
  };
}
