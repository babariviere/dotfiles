{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  options.dotfiles.dev.godot.enable = lib.mkEnableOption "godot";

  config = lib.mkIf cfg.dev.godot.enable {
    environment.systemPackages = with pkgs;
      [ pkgs.nur.repos.babariviere.godot ];
  };
}
