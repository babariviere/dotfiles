{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.tools.build;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ gnumake ];
  };
}
