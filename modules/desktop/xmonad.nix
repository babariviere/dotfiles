{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.xmonad;

in {
  options.dotfiles.desktop.xmonad.enable = lib.mkEnableOption "xmonad";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ xmobar nitrogen trayer ];

    fonts.fonts =
      [ (pkgs.unstable.nerdfonts.override { fonts = [ "Mononoki" ]; }) ];

    services.xserver.displayManager.session = [{
      manage = "desktop";
      name = "xsession";
      start = "exec $HOME/.xsession";
    }];
    home-manager.users."${dotfiles.user}" = {
      home.keyboard = {
        layout = "us";
        variant = "altgr-intl";
      };

      # TODO: clean me
      services.flameshot.enable = true;

      xsession.enable = true;
      xsession.windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
        # TODO: mutate
        config = pkgs.mutate (usrconf "xmonad/xmonad.hs") (dotfiles.theme.colors
          // {
            setWallpaper =
              "${pkgs.feh}/bin/feh --bg-fill ${dotfiles.theme.wallpaper}";
            trayerBackground = "0x"
              + lib.strings.removePrefix "#" dotfiles.theme.colors.background;
          });
      };

      xdg.configFile = {
        "xmobar/xmobarrc".source = pkgs.mutate (usrconf "xmobar/xmobarrc")
          (dotfiles.theme.colors // {
            network = dotfiles.network.wlan or dotfiles.network.eth;
            # TODO: don't use polybar value
            battery = dotfiles.desktop.polybar.battery;
          });
        # TODO: package it
        "xmobar/trayer-padding-icon.sh" = {
          text = ''
            #!/bin/sh
            # Detects the width of running trayer-srg window (xprop name 'panel')
            # and creates an XPM icon of that width, 1px height, and transparent.
            # Outputs an <icon>-tag for use in xmobar to display the generated
            # XPM icon.
            #
            # Run script from xmobar:
            # `Run Com "/where/ever/trayer-padding-icon.sh" [] "trayerpad" 10`
            # and use `%trayerpad%` in your template.


            # Function to create a transparent Wx1 px XPM icon
            create_xpm_icon () {
            timestamp=$(date)
            pixels=$(for i in `seq $1`; do echo -n "."; done)

            cat << EOF > "$2"
            /* XPM *
            static char * trayer_pad_xpm[] = {
            /* This XPM icon is used for padding in xmobar to */
            /* leave room for trayer-srg. It is dynamically   */
            /* updated by by trayer-pad-icon.sh which is run  */
            /* by xmobar.                                     */
            /* <w/cols>  <h/rows>  <colors>  <chars per pixel> */
            "$1 1 1 1",
            /* Colors (none: transparent) */
            ". c none",
            /* Pixels */
            "$pixels"
            };
            EOF
            }

            # Width of the trayer window
            width=$(xprop -name panel | grep 'program specified minimum size' | cut -d ' ' -f 5)

            # Icon file name
            iconfile="/tmp/trayer-padding-''${width}px.xpm"

            # If the desired icon does not exist create it
            if [ ! -f $iconfile ]
            then
                create_xpm_icon $width $iconfile
            fi

            # Output the icon tag for xmobar
            echo "<icon=''${iconfile}/>"
          '';
          executable = true;
        };
        "xmobar/xpm" = {
          source = (usrconf "xmonad/xpm");
          recursive = true;
        };
      };
    };
  };
}
