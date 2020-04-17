{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.firefox;
in {
  options.dotfiles.desktop.firefox.enable = lib.mkEnableOption "firefox";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.variables = { "BROWSER" = "firefox"; };

    home-manager.users."${dotfiles.user}" = {
      programs.firefox = {
        enable = true;
        package = pkgs.firefox;

        profiles."main" = {
          path = "profile.main";
          settings = {
            "browser.tabs.drawInTitlebar" = true;
            "browser.download.dir" =
              config.home-manager.users."${dotfiles.user}".xdg.userDirs.download;
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          };
          # userChrome = ''
          #   @import "${pkgs.sources.firefox-sweet-theme}/userChrome.css";
          # '';
        };
      };
    };
  };
}
