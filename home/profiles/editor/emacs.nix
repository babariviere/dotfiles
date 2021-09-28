{ config, lib, pkgs, ... }:

let emacs = pkgs.amber-emacs;
in lib.mkMerge [
  {
    assertions = [{
      assertion = !config.profiles.editor.doom.enable;
      message = "profiles.editor.doom cannot be enable with emacs.";
    }];

    home.file = {
      ".emacs.d" = {
        source = "${config.dotfiles.configDir}/emacs.d";
        recursive = true;
      };
    };
    home.packages = [ emacs pkgs.emacs-all-the-icons-fonts ]
      ++ (lib.optionals pkgs.stdenv.isDarwin [ pkgs.emacs-client ]);
    programs.emacs.package = emacs;
    shell.aliases = { e = "${emacs}/bin/emacsclient"; };

    shell.env = {
      EDITOR = "${emacs}/bin/emacsclient -nw";
      VISUAL = "${emacs}/bin/emacsclient";
    };

    # Ensure all-the-icons-fonts will be installed
    fonts.fontconfig.enable = true;
  }
  (lib.mkIf pkgs.stdenv.isLinux {
    services.emacs = {
      enable = true;
      package = emacs;
      client.enable = true;
      socketActivation.enable = true;
    };
  })
]
