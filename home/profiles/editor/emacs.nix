{ config, lib, pkgs, ... }:

let
  emacs = pkgs.amber-emacs;
in lib.mkMerge [
  {
    home.file = {
      ".emacs.d" = {
        source = "${config.dotfiles.configDir}/emacs.d";
        recursive = true;
      };
    };
    home.packages =
      [ emacs pkgs.emacs-all-the-icons-fonts pkgs.ripgrep pkgs.biosevka ]
      ++ (lib.optionals pkgs.stdenv.isDarwin [ pkgs.emacs-client ]);
    programs.emacs.package = emacs;
    shell.aliases = { e = "${emacs}/bin/emacsclient -nw"; };

    shell.env = {
      EDITOR = "${emacs}/bin/emacsclient -nw";
      VISUAL = "${emacs}/bin/emacsclient";
    };

    xdg.desktopEntries.org-protocol = {
      name = "Org Protocol";
      exec = "${emacs}/bin/emacsclient %u";
      genericName = "org-protocol";
      #type = "Application";
      mimeType = [ "x-scheme-handler/org-protocol" ];
    };

    # Ensure all-the-icons-fonts will be installed
    fonts.fontconfig.enable = true;
  }
  (lib.mkIf pkgs.stdenv.isLinux {
    services.emacs = {
      enable = true;
      package = emacs;
      client.enable = true;
      startWithUserSession = true;
    };
  })
]
