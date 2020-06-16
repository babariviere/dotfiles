{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.xmonad;

in {
  options.dotfiles.desktop.xmonad.enable = lib.mkEnableOption "xmonad";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ xmobar ];
    home-manager.users."${dotfiles.user}" = {
      xsession.windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
      };

      # xdg.configFile."xmonad/xmonadrc".source = xmonadrc;
      # xdg.configFile."sxhkd/sxhkdrc".source = sxhkdrc;
    };
  };
}
