{ config, lib, pkgs, ... }:

let
  emacs = pkgs.amber-emacs;
  emacs-scratchpad = pkgs.writeScriptBin "emacs-scratch" ''
    ${emacs}/bin/emacs -c -n \
        --eval '(progn (setq frame-title-format "emacs-scratchpad") (amber/org-goto-tasks))'
  '';
  org-protocol = pkgs.makeDesktopItem {
    name = "org-protocol";
    exec = "${emacs}/bin/emacsclient %u";
    comment = "Org protocol";
    desktopName = "org-protocol";
    type = "Application";
    mimeType = "x-scheme-handler/org-protocol";
  };
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
      [ emacs pkgs.emacs-all-the-icons-fonts org-protocol emacs-scratchpad ]
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
