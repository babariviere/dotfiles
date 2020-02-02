{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.android;
in {
  options.dotfiles.dev.android = {
    enable = mkEnableOption "android";
    studio = mkEnableOption "android studio";
  };

  config = mkIf cfg.enable {
    programs.adb.enable = true;

    environment.systemPackages = [ (mkIf cfg.studio pkgs.android-studio) ];

    users.users."${dotfiles.user}".extraGroups = [ "adbusers" ];
  };
}
