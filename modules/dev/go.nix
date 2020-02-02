{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  options.dotfiles.dev.go.enable = lib.mkEnableOption "go";

  config = lib.mkIf cfg.dev.go.enable {
    environment.systemPackages = with pkgs; [ go gomodifytags gotools ];
  };
}
