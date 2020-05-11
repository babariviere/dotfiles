{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.firefox;
in {
  imports = [ ./startpage/sui.nix ];

  options.dotfiles.desktop.firefox.enable = lib.mkEnableOption "firefox";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.variables = { "BROWSER" = "firefox"; };

    dotfiles.desktop.startpage.sui = lib.mkIf dotfiles.tools.docker.enable {
      enable = true;
      apps = [
        {
          name = "Bitwarden";
          url = "bw.babariviere.com";
          icon = "shield-lock-outline";
        }
        {
          name = "Plex";
          url = "plex.tv";
          icon = "plex";
        }
        {
          name = "Gitea";
          url = "git.babariviere.com";
          icon = "git";
        }
        {
          name = "GitHub";
          url = "github.com";
          icon = "github";
        }
        {
          name = "Drone";
          url = "ci.babariviere.com";
          icon = "test-tube";
        }
      ];

      bookmarks = [
        {
          category = "Communication";
          links = [
            {
              name = "Discord";
              url = "https://discordapp.com";
            }
            {
              name = "Gmail";
              url = "https://gmail.com";
            }
          ];
        }
        {
          category = "Media";
          links = [
            {
              name = "YouTube";
              url = "https://youtube.com";
            }
            {
              name = "Spotify";
              url = "https://browse.spotify.com";
            }
          ];
        }
        {
          category = "Reading";
          links = [
            {
              name = "Medium";
              url = "http://medium.com";
            }
            {
              name = "Reddit";
              url = "http://reddit.com";
            }
          ];
        }
      ];

      providers = [
        {
          name = "Duck Duck Go";
          prefix = "/d";
          url = "https://duckduckgo.com/?q=";
        }
        {
          name = "Reddit";
          prefix = "/r";
          url = "https://www.reddit.com/search?q=";
        }
        {
          name = "Qwant";
          prefix = "/q";
          url = "https://www.qwant.com/?q=";
        }
        {
          name = "Soundcloud";
          prefix = "/so";
          url = "https://soundcloud.com/search?q=";
        }
        {
          name = "Spotify";
          prefix = "/s";
          url = "https://open.spotify.com/search/results/";
        }
      ];
    };

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
            "browser.startup.page" = 3; # Restore previous sessions
            "browser.uidensity" = 1;
            "identity.fxaccounts.account.device.name" =
              config.networking.hostName;
            "privacy.trackingprotection.enabled" = true;
            "privacy.trackingprotection.socialtracking.enabled" = true;
            "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
            "services.sync.engine.passwords" = false;
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          } // lib.mkIf dotfiles.desktop.startpage.sui.enable {
            "browser.startup.homepage" = "http://localhost:${
                builtins.toString dotfiles.desktop.startpage.sui.port
              }";
          };
          # userChrome = ''
          #   @import "${pkgs.sources.moonlight-userChrome}/userChrome.css";
          # '';
        };
      };
    };
  };
}
