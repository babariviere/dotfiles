{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.shell.zsh;
  zgen = builtins.fetchGit "https://github.com/tarjoilija/zgen";
in {
  config = lib.mkIf cfg.enable {
    environment = {
      variables = {
        ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
        ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
        ZGEN_DIR = "$XDG_CACHE_HOME/zgen";
        ZGEN_SOURCE = "${zgen}";
      };
      systemPackages = with pkgs; [ zsh nix-zsh-completions fd exa htop tree ];
    };

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableGlobalCompInit = true;
      promptInit = "";
    };

    users.users."${dotfiles.user}".shell = pkgs.zsh;

    home-manager.users."${dotfiles.user}".xdg.configFile = {
      "zsh" = {
        source = <config/zsh>;
        recursive = true;
      };
    };
  };
}
