{ config, lib, pkgs, ... }:

let
  cfg = config.services.dotfiles;
in
{
  require = [
    ../.
  ];

  environment = {
    systemPackages = with pkgs; [
      zsh
      nix-zsh-completions
      fd
      exa
      htop
      tree
    ];
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableGlobalCompInit = true;
  };

  users.users."${cfg.user}".shell = pkgs.zsh;
}
