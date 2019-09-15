{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.emacs;
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

    direnv = mkOption {
      type = types.bool;
      description = "Add support for direnv";
      default = true;
    };

    ripgrep = mkOption {
      type = types.bool;
      description = "Add support for ripgrep";
      default = true;
    };
    
    rls = mkOption {
      type = types.bool;
      description = "Add support for rls";
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
        (mkIf (cfg.direnv) direnv) # :tools direnv
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
  };
}
