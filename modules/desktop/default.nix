{ config, lib, pkgs, ... }:

with lib;
let
  unstable = import <nixpkgs-unstable> { };
  cfg = config.dotfiles.desktop;
  withLigature = cfg.ligature;
  iosevkaBaba = (unstable.pkgs.iosevka.override {
    privateBuildPlan = {
      design = if withLigature then
        [ "ligset-haskell" ]
      else
        [ ] ++ [
          "v-at-fourfold"
          "v-a-singlestorey"
          "v-i-zshaped"
          "v-g-singlestorey"
          "v-l-zshaped"
          "v-brace-straight"
          "v-numbersign-slanted"
          "v-asterisk-hexlow"
        ];
      family = "Iosevka Baba";
    };

    set = "baba";
    extraParameters =
      if withLigature then builtins.readFile ./iosevka.toml else null;
    # TODO: extraParameters
  });
  iosevkaTermBaba = (unstable.pkgs.iosevka.override {
    privateBuildPlan = {
      design = [
        "term"
        "v-at-fourfold"
        "v-a-singlestorey"
        "v-i-zshaped"
        "v-g-singlestorey"
        "v-l-zshaped"
        "v-brace-straight"
        "v-numbersign-slanted"
        "v-asterisk-hexlow"
      ];
      family = "Iosevka Term Baba";
    };
    set = "term-baba";
  });
in {
  imports = [
    ./bspwm.nix
    ./i3.nix
    ./programs/chrome.nix
    ./programs/compton.nix
    ./programs/dunst.nix
    ./programs/firefox.nix
    ./programs/polybar.nix
    ./programs/rofi.nix
    ./programs/termite.nix
    ./programs/thunderbird.nix
  ];

  options.dotfiles.desktop = {
    enable = mkEnableOption "desktop";

    ligature = mkOption {
      type = types.bool;
      description = "Add ligature support to all services";
      default = true;
    };

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

    # TODO: generate with IosevkaGenHS
    fonts = {
      fonts = [
        iosevkaBaba
        iosevkaTermBaba
        pkgs.nur.repos.babariviere.nerd-font-symbols
      ];
      enableDefaultFonts = true;
    };

    home-manager.users."${config.dotfiles.user}" = {
      home = {
        file = {
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
      };
      gtk = {
        enable = true;
        font = {
          name = "Roboto 12";
          package = pkgs.roboto;
        };
        iconTheme = {
          package = pkgs.paper-icon-theme;
          name = "Paper";
        };
        theme = {
          package = pkgs.materia-theme;
          name = "Materia-dark";
        };
      };
    };
  };
}
