{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.editors.emacs;
  rg = (pkgs.ripgrep.override { withPCRE2 = true; });
  unstable = import <nixpkgs-unstable> { };
in {
  options.dotfiles.editors.emacs = {
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
          unstable.emacs
          (mkIf (cfg.editorconfig) editorconfig-core-c) # :tools editorconfig

          # Misc
          texlive.combined.scheme-medium # :lang org
          (mkIf (cfg.ripgrep) rg)

        ] ++ (if cfg.spellcheck then [
          aspell
          aspellDicts.en
          aspellDicts.en-computers
          aspellDicts.fr
        ] else
          [ ]);

      sessionVariables = { EDITOR = "emacs"; };

      shellAliases = { e = "emacs"; };
    };

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    home-manager.users."${dotfiles.user}" = {
      home.file.".doom.d" = {
        source = <config/emacs/doom.d>;
        recursive = true;
      };
      xdg.configFile = {
        "zsh/rc.d/env.emacs.zsh".source = <config/emacs/env.zsh>;
      };
    };
  };
}
