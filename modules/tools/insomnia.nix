{ config, lib, pkgs, ... }:

let cfg = config.services.dotfiles.tools.insomnia;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ insomnia ];
  };
}
