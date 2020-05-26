{ config, lib, pkgs, usrconf, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.shell.zsh;
in {
  options.dotfiles.shell.zsh = {
    enable = mkEnableOption "zsh";
    package = mkOption {
      type = types.package;
      description = "Package to use for zsh.";
      default = pkgs.zsh;
    };
    default = mkOption {
      type = types.bool;
      description = "Set the shell as default";
      default = true;
    };
  };

  config = mkIf cfg.enable {
    environment = {
      variables = {
        ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
        ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
        ZGEN_DIR = "$XDG_CACHE_HOME/zgen";
        ZGEN_SOURCE = "${pkgs.inputs.zgen}";
      };
      systemPackages = with pkgs; [ cfg.package h nix-zsh-completions ];
    };

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableGlobalCompInit = true;
    };
    users.users."${dotfiles.user}".shell = mkIf cfg.default cfg.package;

    home-manager.users."${dotfiles.user}" = {
      programs.zsh = {
        enable = true;
        dotDir = ".config/zsh";
        envExtra = ''
          for file in $XDG_CONFIG_HOME/zsh/rc.d/env.*.zsh(N); do
            source $file
          done

          export CHROME_EXECUTABLE=google-chrome-stable
          export TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'
        '';
        initExtra = ''
          ${pkgs.any-nix-shell}/bin/any-nix-shell zsh | source /dev/stdin
          source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
          eval "$(h --setup ~/src)"
          source $ZDOTDIR/init.zsh
        '';
      };
      xdg.configFile = {
        "zsh" = {
          source = (usrconf "zsh");
          recursive = true;
        };
      };
    };
  };
}
