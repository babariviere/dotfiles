{
  programs.waybar = {
    enable = true;
    settings = [{
      layer = "top";
      position = "top";
      height = 30;
      # output = [ ];
      modules-left = [ "sway/workspaces" "sway/mode" ];
      modules-center = [ "clock" ];
      modules-right = [ "tray" "pulseaudio" "network" "battery" ];
      modules = {
        "sway/mode" = { "format" = ''<span style="italic">{}</span>''; };
        network = {
          interface = "wl*";
          format-wifi = "{essid} ({signalStrength}%) ";
          format-ethernet = "{ifname} ";
          format-disconnected = "";
          max-length = 50;
          # "on-click" = "foot -e 'nmtui'";
        };
        tray = {
          icon-size = 15;
          spacing = 10;
        };
        battery = {
          states = {
            good = 95;
            warning = 20;
            critical = 10;
          };
          format = "{capacity}% {icon}";
          format-charging = "{capacity}% ";
          format-plugged = "{capacity}% ";
          format-alt = "{time} {icon}";
          format-full = "";
          format-icons = [ "" "" "" "" "" ];
        };
        pulseaudio = {
          format = "{volume}% {icon} ";
          format-bluetooth = "{volume}% {icon} {format_source}";
          format-bluetooth-muted = " {icon} {format_source}";
          format-muted = " ";
          format-source = "{volume}% ";
          format-source-muted = "";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = [ "" "" "" ];
          };
          # "on-click" = "pavucontrol";
        };
      };
    }];
    style = ''
      * {
          font-family: all-the-icons, MonoLisa;
          font-size: 13px;
          border: none;
          border-radius: 0;
          min-height: 0;
          box-shadow:    none;
          text-shadow:   none;
          transition-duration: 0s;
      }

      window {
          color:      rgba(217, 216, 216, 1);
          background: rgba(35, 31, 32, 0.00);
      }

      window#waybar.solo {
          color:      rgba(217, 216, 216, 1);
          background: rgba(35, 31, 32, 0.85);
      }

      #workspaces {
          margin: 0 2px;
      }

      #workspaces button {
          padding:    0 5px;
          color:      rgba(217, 216, 216, 0.4);
          background: rgba(0, 0, 0, 0.0);
      }

      #workspaces button.visible {
          color:      rgba(217, 216, 216, 1);
      }

      #workspaces button.focused {
          border-top: 3px solid rgba(217, 216, 216, 1);
          border-bottom: 3px solid rgba(217, 216, 216, 0);
      }

      #workspaces button.urgent {
          color:      rgba(238, 46, 36, 1);
      }

      #mode, #battery, #cpu, #memory, #network, #pulseaudio, #idle_inhibitor, #backlight, #custom-storage, #custom-spotify, #custom-weather, #custom-mail {
          margin:     0px 6px 0px 10px;
          min-width:  25px;
      }

      #clock {
          margin:     0px 16px 0px 10px;
          min-width:  140px;
      }

      #battery.warning {
      color:       rgba(255, 210, 4, 1);
      }

      #battery.critical {
          color:      rgba(238, 46, 36, 1);
      }

      #battery.charging {
          color:      rgba(217, 216, 216, 1);
      }

      #pulseaudio.muted {
          font-family: github-octicons;
      }
    '';
  };
}
