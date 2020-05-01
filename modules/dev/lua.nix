{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.lua;
in {
  options.dotfiles.dev.lua.enable = lib.mkEnableOption "lua";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ lua luarocks ];
  };
}
