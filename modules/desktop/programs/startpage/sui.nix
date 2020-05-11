{ config, lib, pkgs, ... }:

with lib;

let
  dotfiles = config.dotfiles;
  sui = dotfiles.desktop.startpage.sui;
in {
  options.dotfiles.desktop.startpage.sui = with types; {
    enable = mkEnableOption "sui";

    src = mkOption {
      type = path;
      default = pkgs.sources.sui;
      description = "Source code for sui.";
    };

    port = mkOption {
      type = port;
      default = 8402;
      description = "Port to run sui on.";
    };

    apps = mkOption {
      type = listOf (submodule {
        options = {
          name = mkOption {
            type = str;
            description = "App's name.";
          };

          url = mkOption {
            type = str // { check = (x: !lib.hasPrefix "http" x); };
            description = "URL of the application.";
          };

          icon = mkOption {
            type = str;
            description =
              "Icon name. List of icons can be found here: https://materialdesignicons.com/";
          };
        };
      });
      description = "List of applications.";
      default = [ ];
    };

    bookmarks = mkOption {
      type = listOf (submodule {
        options = {
          category = mkOption {
            type = str;
            description = "Category name.";
          };

          links = mkOption {
            type = listOf (submodule {
              options = {
                name = mkOption {
                  type = str;
                  description = "Bookmark name.";
                };

                url = mkOption {
                  type = str;
                  description = "URL of the bookmark.";
                };
              };
            });
          };
        };
      });
      description = "A list of bookmark.";
      default = [ ];
    };

    providers = mkOption {
      type = listOf (submodule {
        options = {
          name = mkOption {
            type = str;
            description = "Provider name.";
          };
          url = mkOption {
            type = str;
            description = "URL of the provider for search.";
            example = "https://duckduckgo.com/?q=";
          };
          prefix = mkOption {
            type = str;
            description = "Prefix to use to search from this provider.";
            example = "d";
          };
        };
      });
      description = "A list of provider.";
      default = [ ];
    };
  };

  config = mkIf sui.enable {
    docker-containers = lib.mkIf dotfiles.tools.docker.enable {
      sui = {
        image = "nginx:alpine";
        ports = [ "${builtins.toString sui.port}:80" ];
        volumes = let
          apps =
            pkgs.writeText "apps.json" (builtins.toJSON { apps = sui.apps; });
          links = pkgs.writeText "links.json"
            (builtins.toJSON { bookmarks = sui.bookmarks; });
          providers = pkgs.writeText "providers.json"
            (builtins.toJSON { providers = sui.providers; });
          src = pkgs.runCommand "sui" { } ''
            mkdir -p $out
            cp -r ${sui.src}/assets $out/assets
            cp ${sui.src}/index.html $out/index.html
            cp ${apps} $out/apps.json
            cp ${links} $out/links.json
            cp ${providers} $out/providers.json
          '';

        in [ "${src}:/usr/share/nginx/html" ];
      };
    };
  };
}
