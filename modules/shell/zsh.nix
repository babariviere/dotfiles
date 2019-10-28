{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.shell.zsh;
in {
  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [ zsh nix-zsh-completions fd exa htop tree ];
    };

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableGlobalCompInit = true;
    };

    users.users."${dotfiles.user}".shell = pkgs.zsh;
  };
}
