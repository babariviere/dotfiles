{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  config = lib.mkIf cfg.dev.godot.enable {
    environment.systemPackages = with pkgs;
      [ pkgs.nur.repos.babariviere.godot ];
  };
}
