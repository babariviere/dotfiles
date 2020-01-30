{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.php;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      php
      php-unit
      phpPackages.composer
    ];

    home-manager.users."${dotfiles.user}" = {
      xdg.configFile = {
        "zsh/rc.d/env.php.zsh".source = <config/php/env.zsh>;
        "composer/composer.json".source = <config/php/composer/composer.json>;
      };
    };
  };
}
