{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.shell.zsh;
in {
  options.dotfiles.shell.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf cfg.enable {
    environment = {
      variables = {
        ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
        ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
        ZGEN_DIR = "$XDG_CACHE_HOME/zgen";
        ZGEN_SOURCE = "${pkgs.sources.zgen}";
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
