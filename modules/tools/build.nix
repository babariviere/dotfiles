{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.tools.build;
in {
  options.dotfiles.tools.build.enable = lib.mkEnableOption "build";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ gnumake ];
  };
}
