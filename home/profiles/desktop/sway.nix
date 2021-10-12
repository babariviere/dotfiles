{ config, lib, pkgs }:

{
  profiles.desktop = {
    foot.enable = true;
    mako.enable = true;
    rofi.enable = true;
    waybar.enable = true;
  };

  wayland.windowManager.sway = {
    enable = true;
    xwayland = true;
    wrapperFeatures.gtk = true;
    config = {
      bars = [{
        position = "top";
        command = "${pkgs.waybar}/bin/waybar";
      }];
      colors = {
        background = "#F8F8F2";
        focused = {
          border = "#6272A4";
          background = "#6272A4";
          text = "#F8F8F2";
          indicator = "#6272A4";
          childBorder = "#6272A4";
        };
        focusedInactive = {
          border = "#44475A";
          background = "#44475A";
          text = "#F8F8F2";
          indicator = "#44475A";
          childBorder = "#44475A";
        };
        unfocused = {
          border = "#282A36";
          background = "#282A36";
          text = "#BFBFBF";
          indicator = "#282A36";
          childBorder = "#282A36";
        };
        urgent = {
          border = "#44475A";
          background = "#FF5555";
          text = "#F8F8F2";
          indicator = "#FF5555";
          childBorder = "#FF5555";
        };
        placeholder = {
          border = "#282A36";
          background = "#282A36";
          text = "#F8F8F2";
          indicator = "#282A36";
          childBorder = "#282A36";
        };
      };
      focus.followMouse = false;
      fonts = {
        names = [ "MonoLisa" ];
        style = "Regular";
        size = 10.0;
      };
      input = {
        "*" = {
          xkb_layout = "us";
          xkb_variant = "altgr-intl";
          xkb_options = "caps:ctrl_modifier";
        };
        "1267:13:Elan_Touchpad" = {
          click_method = "clickfinger";
          dwt = "enabled";
          natural_scroll = "enabled";
          tap = "disabled";
        };
      };
      output = let
        artworks = pkgs.fetchFromGitHub {
          owner = "NixOS";
          repo = "nixos-artwork";
          rev = "9bd73014f75c2ce97d104c78314d78eb2493e24d";
          sha256 = "1976vc2w26h78wngqm7q1rnqa387y1bpf9wicxfghbc2qkimq2m5";
        };
        bg = "${artworks}/wallpapers/nix-wallpaper-dracula.png fill";
      in {
        eDP-1 = {
          pos = "0 0";
          inherit bg;
        };
        HDMI-A-1 = {
          pos = "1920 0";
          inherit bg;
        };
      };
      modifier = "Mod4";
      terminal = "${pkgs.foot}/bin/foot";
      keybindings = let
        resize = pkgs.writeScript "resize" ''
          visible=$(${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq '.nodes[].nodes[].floating_nodes[] | select(.name == "emacs-scratchpad") | .visible?')
          if [ "$visible" = "true" ]; then
            ${pkgs.sway}/bin/swaymsg '[title="emacs-scratchpad" floating]' resize set 90 ppt 90 ppt, move position center
          fi
        '';
      in lib.mkOptionDefault {
        "Mod4+End" =
          "exec ${pkgs.swaylock-effects}/bin/swaylock --screenshots --clock --effect-blur 7x5 --effect-vignette 0.5:0.5 --grace 2 --fade-in 0.2";
        "Mod4+d" = ''
          exec ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop --dmenu "${pkgs.rofi}/bin/rofi -dmenu -i -show-icons"
        '';
        "Mod4+minus" = "scratchpad show, exec ${resize}";
        "Mod4+less" = "exec ${pkgs.mako}/bin/makoctl dismiss";
        "Mod4+greater" = "exec ${pkgs.mako}/bin/makoctl invoke";
        "XF86AudioRaiseVolume" = "exec ${pkgs.pamixer}/bin/pamixer -i 2";
        "XF86AudioLowerVolume" = "exec ${pkgs.pamixer}/bin/pamixer -d 2";
        "XF86AudioMute" = "exec ${pkgs.pamixer}/bin/pamixer -t";
        "XF86MonBrightnessDown" =
          "exec ${pkgs.brightnessctl}/bin/brightnessctl s 5%-";
        "XF86MonBrightnessUp" =
          "exec ${pkgs.brightnessctl}/bin/brightnessctl s +5%";
      };
      startup = [
        {
          # Import variables needed for screen sharing and gnome3 pinentry to work.
          command =
            "${pkgs.dbus}/bin/dbus-update-activation-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway";
        }
        { command = "${pkgs.systemd}/bin/systemctl --user import-environment"; }
        {
          command = let
            inherit (pkgs) glib;
            gnome-schema = "org.gnome.desktop.interface";
          in ''
            {
              ${glib.bin}/bin/gsettings set ${gnome-schema} gtk-theme '${config.gtk.theme.name}'
              ${glib.bin}/bin/gsettings set ${gnome-schema} icon-theme '${config.gtk.iconTheme.name}'
              ${glib.bin}/bin/gsettings set ${gnome-schema} cursor-theme '${config.xsession.pointerCursor.name}'
              ${glib.bin}/bin/gsettings set ${gnome-schema} font-name '${config.gtk.font.name}'
            }
          '';
          always = true;
        }
      ] ++ (lib.optionals config.profiles.editor.emacs.enable [{
        command = "emacs-scratch";
      }]);

      window.commands = [
        {
          command = ''
            floating enable, resize set 90 ppt 90 ppt, move position center, move to scratchpad
          '';
          criteria = { title = "emacs-scratchpad"; };
        }
        {
          command = ''
            resize set 90 ppt 90 ppt, move position center
          '';
          criteria = {
            title = "emacs-scratchpad";
            floating = true;
          };
        }
      ];
    };
    extraConfig = ''
      default_border pixel 2
    '';
  };

  services.xsettingsd = {
    enable = true;
    settings = {
      "Gtk/CursorThemeName" = config.xsession.pointerCursor.name;
      "Net/IconThemeName" = config.gtk.iconTheme.name;
      "Net/ThemeName" = config.gtk.theme.name;
    };
  };
}
