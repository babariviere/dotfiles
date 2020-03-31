{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.shell.fish;
in {
  options.dotfiles.shell.fish = {
    enable = mkEnableOption "fish";
    package = mkOption {
      type = types.package;
      description = "Package to use for fish.";
      default = pkgs.fish;
    };
    default = mkOption {
      type = types.bool;
      description = "Set the shell as default";
      default = false; # TODO: i don't like how it's done
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    programs.fish.enable = true;
    users.users."${dotfiles.user}".shell = mkIf cfg.default cfg.package;

    home-manager.users."${dotfiles.user}" = {
      programs.fish = {
        enable = true;
        loginShellInit = ''
          set -U fish_greeting

          set -x CHROME_EXECUTABLE google-chrome-stable
        '';
        promptInit = ''
          for file in $XDG_CONFIG_HOME/fish/rc.d/env.*.fish
            source $file
          end

          ${pkgs.any-nix-shell}/bin/any-nix-shell fish | source
        '';
      };
      # xdg.configFile = {
      #   "fish" = {
      #     source = <config/fish>;
      #     recursive = true;
      #   };
      # };
    };
  };
}
