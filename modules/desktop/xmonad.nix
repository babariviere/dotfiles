{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.xmonad;

in {
  options.dotfiles.desktop.xmonad.enable = lib.mkEnableOption "xmonad";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ xmobar ];

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
        config = (usrconf "xmonad/xmonad.hs");
      };

      xdg.configFile = {
        "xmobar/xmobarrc".source = (usrconf "xmobar/xmobarrc");
        "xmobar/xpm" = {
          source = (usrconf "xmonad/xpm");
          recursive = true;
        };
      };
    };
  };
}
