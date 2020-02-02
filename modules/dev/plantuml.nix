{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.dev.plantuml;
in {
  options.dotfiles.dev.plantuml.enable = lib.mkEnableOption "plantuml";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ plantuml graphviz ];
  };
}
