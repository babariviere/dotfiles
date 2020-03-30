{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.dotfiles.desktop;
  cursorTheme = {
    package = pkgs.bibata-cursors;
    name = "Bibata Ice";
  };
  iconTheme = {
    package = pkgs.paper-icon-theme;
    name = "Paper";
  };
  theme = {
    package = pkgs.materia-theme;
    name = "Materia-dark";
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
          name = "${cfg.fonts.sansSerif.name} 12";
          package = cfg.fonts.sansSerif.package;
        };
        inherit iconTheme theme;
      };

      xsession.pointerCursor = cursorTheme;
    };
  };
}
