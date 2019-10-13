{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.emacs;
  dotfiles = config.services.dotfiles;
  rg = (pkgs.ripgrep.override {withPCRE2 = true;});
in
{
  # TODO: install doom config via home-manager
  options.services.emacs = {
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
  };
  
  config = {
    environment = {
      systemPackages = with pkgs; [
        (mkIf (config.programs.gnupg.agent.enable) pinentry_emacs)

        # Essential
        emacs
        (mkIf (cfg.editorconfig) editorconfig-core-c) # :tools editorconfig

        # Misc
        texlive.combined.scheme-medium # :lang org
        (mkIf (cfg.ripgrep) rg)
      ];

      sessionVariables = {
        EDITOR = "emacs";
      };

      shellAliases = {
        e = "emacs";
      };
    };

    fonts.fonts = [pkgs.emacs-all-the-icons-fonts];


    home-manager.users."${dotfiles.user}".xdg.configFile = {
      "zsh/rc.d/env.emacs.zsh".source = <config/emacs/env.zsh>;
    };
  };
}
