{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.dev.plantuml;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ plantuml graphviz ];
  };
}
