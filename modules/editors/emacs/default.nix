{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.editors.emacs;
  rg = (pkgs.ripgrep.override { withPCRE2 = true; });

  emacs' = pkgs.emacsGit.overrideAttrs
    (old: { buildInputs = old.buildInputs ++ [ pkgs.harfbuzz.dev ]; });
  editorScript = pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    if [ -z "$1" ]; then
      exec ${emacs'}/bin/emacsclient --create-frame --alternate-editor ${emacs'}/bin/emacs
    else
      exec ${emacs'}/bin/emacsclient --alternate-editor ${emacs'}/bin/emacs "$@"
    fi
  '';
in {
  options.dotfiles.editors.emacs = {
    enable = mkEnableOption "emacs";

    daemon = mkOption {
      type = types.bool;
      description = "Enable emacs daemon for each user.";
      default = true;
    };

    completion = mkOption {
      type = types.enum [ "ivy" "ido" "helm" ];
      description = "Completion framework to use in emacs.";
      default = "ivy";
    };

    terminal = mkOption {
      type = types.enum [ "eshell" "shell" "term" "vterm" ];
      default = "eshell";
    };

    editorconfig = mkOption {
      type = types.bool;
      description = "Add support for editorconfig";
      default = true;
    };

    ripgrep = mkOption {
      type = types.bool;
      description = "Add support for ripgrep";
      default = true;
    };

    spellcheck = mkOption {
      type = types.bool;
      description = "Add support for spellcheck";
      default = true;
    };
  };

  # TODO: install doom config via home-manager
  config = mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs;
        [
          (mkIf (config.programs.gnupg.agent.enable) pinentry_emacs)

          # Essential
          # emacsGit
          emacs'
          (mkIf (cfg.editorconfig) editorconfig-core-c) # :tools editorconfig

          # For launching emacs lisp tests
          ert-run

          # Misc
          ccls # lang cc

          pandoc # :lang org
          wkhtmltopdf
          (mkIf (cfg.ripgrep) rg)

          plantuml # :lang plantuml

          shellcheck # :lang sh

          nodePackages.prettier

          languagetool # to speak correctly

        ] ++ optionals cfg.spellcheck [
          aspell
          aspellDicts.en
          aspellDicts.en-computers
          aspellDicts.fr
        ];

      shellAliases = { e = "${editorScript}/bin/emacseditor -nw"; };

      variables = {
        EDITOR = "${editorScript}/bin/emacseditor -nw";
        VISUAL = "${editorScript}/bin/emacseditor -nw";
      };
    };

    services.emacs = mkIf cfg.daemon {
      enable = true;
      defaultEditor = false;
      package = emacs';
    };

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    home-manager.users."${dotfiles.user}" = {
      home.file = {
        ".doom.d/modules" = {
          source = <config/emacs/doom.d/modules>;
          recursive = true;
        };
        ".doom.d/config.el".source =
          pkgs.mutate <config/emacs/doom.d/config.el> {
            doomTheme = dotfiles.doomTheme;
            font = dotfiles.desktop.fonts.mono.name;
          };
        ".doom.d/init.el".text = import ./init.el.nix { inherit config lib; };
        ".doom.d/packages.el".source = <config/emacs/doom.d/packages.el>;
      };
      xdg.configFile = {
        "zsh/rc.d/env.emacs.zsh".source = <config/emacs/env.zsh>;
      };
    };
  };
}
