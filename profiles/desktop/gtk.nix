{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop;
  cursorTheme = {
    package = pkgs.bibata-cursors;
    name = "Bibata Ice";
  };
  iconTheme = {
    package = pkgs.paper-icon-theme;
    name = "Paper";
  };
  theme = {
    package = pkgs.plata-theme;
    name = "Plata-Noir";
  };
in {
  config = mkIf cfg.enable {
    services.xserver.displayManager.lightdm.greeters.gtk = with pkgs; {
      inherit cursorTheme iconTheme theme;
    };

    home-manager.users."${config.dotfiles.user}" = {
      gtk = {
        enable = true;
        font = {
          name = "${dotfiles.theme.fonts.sansSerif.name} 12";
          package = dotfiles.theme.fonts.sansSerif.package;
        };
        inherit iconTheme theme;
      };

      qt = {
        enable = true;
        platformTheme = "gtk";
      };

      xsession.pointerCursor = cursorTheme;
    };
  };
}
