{ config, pkgs }:

{
  profiles.desktop = {
    alacritty.enable = true;
    picom.enable = true;
    xmobar.enable = true;
  };

  home.packages = with pkgs; [ rofi biosevka ];

  xdg.configFile."xmonad/xmonad.hs".source = config.lib.file.mkOutOfStoreSymlink
    "${config.dotfiles.configDir}/xmonad/xmonad.hs";
}
