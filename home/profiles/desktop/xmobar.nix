{ config, pkgs }:

{
  home.packages = [
    pkgs.alsa-utils
    pkgs.trayer
  ];

  xdg.configFile = {
    "xmobar/xmobarrc0".source = "${config.dotfiles.configDir}/xmobar/xmobarrc0";
    "xmobar/xmobarrc1".source = "${config.dotfiles.configDir}/xmobar/xmobarrc1";
    "xmobar/trayer-padding.sh" = {
      source = "${config.dotfiles.configDir}/xmobar/trayer-padding.sh";
      executable = true;
    };
  };
}
