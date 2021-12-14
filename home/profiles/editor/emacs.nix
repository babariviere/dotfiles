{ config, lib, pkgs, ... }:

let
  emacs = pkgs.amber-emacs;
  emacs-scratchpad = pkgs.writeScriptBin "emacs-scratch" ''
    #!/bin/sh
    ${emacs}/bin/emacsclient -c -n \
        --eval '(progn (set-frame-parameter nil (quote title) "emacs-scratchpad") (amber/org-goto-tasks))'
  '';
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
    home.packages =
      [ emacs pkgs.emacs-all-the-icons-fonts emacs-scratchpad pkgs.ispell ]
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
      socketActivation.enable = true;
    };
  })
]
