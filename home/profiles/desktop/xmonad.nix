{ config }:

{
  home.file.".xmonad/xmonad.hs".source = config.lib.file.mkOutOfStoreSymlink "${config.dotfiles.configDir}/xmonad/xmonad.hs";
}
