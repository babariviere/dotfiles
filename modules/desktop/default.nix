{ config, lib, pkgs, ... }:

with lib;
let cfg = config.dotfiles.desktop;
in {
  imports = [
    ./bspwm.nix
    ./fonts.nix
    ./gtk.nix
    ./i3.nix
    ./programs/chrome.nix
    ./programs/compton.nix
    ./programs/dunst.nix
    ./programs/firefox.nix
    ./programs/picom.nix
    ./programs/polybar.nix
    ./programs/rofi.nix
    ./programs/termite.nix
    ./programs/thunderbird.nix
  ];

  options.dotfiles.desktop = {
    enable = mkEnableOption "desktop";

    xinitCmd = mkOption {
      type = types.str;
      description = "launch command for xinitrc";
    };

  };

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

    home-manager.users."${config.dotfiles.user}" = {
      home.file = {
        ".xinitrc".text = ''
          #!/bin/sh

          if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
            eval $(dbus-launch --exit-with-session --sh-syntax)
          fi
          systemctl --user import-environment DISPLAY XAUTHORITY

          if command -v dbus-update-activation-environment >/dev/null 2>&1; then
                  dbus-update-activation-environment DISPLAY XAUTHORITY
          fi

          for f in $HOME/.Xresources.d/*; do
              ${pkgs.xorg.xrdb}/bin/xrdb -merge "$f"
          done

          ${cfg.xinitCmd}
        '';

        ".xprofile" = {
          executable = true;
          text = ''
            #!/bin/sh

            if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
              eval $(dbus-launch --exit-with-session --sh-syntax)
            fi
            systemctl --user import-environment DISPLAY XAUTHORITY

            if command -v dbus-update-activation-environment >/dev/null 2>&1; then
                    dbus-update-activation-environment DISPLAY XAUTHORITY
            fi
          '';
        };
      };
      # xsession.enable = true;
    };
  };
}
