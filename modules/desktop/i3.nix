{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.i3;

  screenshot = pkgs.writeShellScriptBin "screenshot" ''
    filename=$(date '+%Y-%m-%d-%H-%M-%S').png
    file=$HOME/Pictures/screenshot/$filename
    mkdir -p $(dirname $file)
    ${pkgs.maim}/bin/maim $@ $file && ${pkgs.libnotify}/bin/notify-send "Screenshot taken" "saved into $file"
  '';

  configFile = pkgs.mutate <config/i3/config> (dotfiles.colors // {
    setWallpaper = "${pkgs.feh}/bin/feh --bg-center ${dotfiles.wallpaper}";
    screenshot = "${screenshot}/bin/screenshot";
  });
in {
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    services.xserver.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = with pkgs; [
        i3lock-color
        betterlockscreen
        rofi
        feh
        maim # for screenshot
        screenshot
      ];
    };

    dotfiles.desktop.xinitCmd = lib.mkForce "exec i3";

    home-manager.users."${dotfiles.user}" = {
      xdg.configFile."i3/config".source = configFile;
    };
  };
}
