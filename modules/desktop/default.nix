{ config, lib, pkgs, ... }:

with lib;
let cfg = config.dotfiles.desktop;
in {
  options.dotfiles.desktop.enable = mkEnableOption "desktop";

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "altgr-intl";
      displayManager.startx.enable =
        false; # TODO: use services.xserver.windowManager.session
      desktopManager.xterm.enable = false;
      displayManager.lightdm = {
        enable = true;
        # greeters.mini.enable = true; TODO: find a better greeter -> look at webkit
      };
      libinput = {
        enable = true;
        tapping = false;
        disableWhileTyping = true;
        naturalScrolling = true;
      };
    };

    services.dbus.packages = with pkgs; [ gnome3.dconf ];

    sound.enable = true;
    hardware.pulseaudio.enable = true;

    environment.systemPackages = with pkgs; [ xclip pamixer pavucontrol ];

    home-manager.users.${config.dotfiles.user}.xresources.properties =
      with lib.attrsets;
      mapAttrs' (k: v: nameValuePair ("*" + k) v)
      config.dotfiles.theme.colorsAnsi;
  };
}
