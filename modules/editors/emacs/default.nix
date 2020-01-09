{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.editors.emacs;
  rg = (pkgs.ripgrep.override { withPCRE2 = true; });
  unstable = import <nixpkgs-unstable> { };

  emacs' = unstable.emacs;

in {
  options.dotfiles.editors.emacs = {
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
    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
      }))
    ];

    environment = {
      systemPackages = with pkgs;
        [
          (mkIf (config.programs.gnupg.agent.enable) pinentry_emacs)

          # Essential
          # emacsGit
          emacs'
          (mkIf (cfg.editorconfig) editorconfig-core-c) # :tools editorconfig

          # Misc
          ccls # lang cc

          pandoc # :lang org
          wkhtmltopdf
          (mkIf (cfg.ripgrep) rg)

          plantuml # :lang plantuml

          shellcheck # :lang sh

          nodePackages.prettier

        ] ++ optionals cfg.spellcheck [
          aspell
          aspellDicts.en
          aspellDicts.en-computers
          aspellDicts.fr
        ];

      shellAliases = {
        e = if cfg.daemon then config.environment.variables.EDITOR else "emacs";
      };
    };

    services.emacs = mkIf cfg.daemon {
      enable = true;
      defaultEditor = true;
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
