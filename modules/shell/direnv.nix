{ config, pkgs, lib, ... }:

let
  cfg = config.dotfiles.shell.direnv;
  sources = import
    ../../nix/sources.nix; # cannot use pkgs.unstable directly in import (infinite recursion)
in {
  disabledModules = [ "services/development/lorri.nix" ];
  imports =
    [ "${sources.unstable}/nixos/modules/services/development/lorri.nix" ];

  options.dotfiles.shell.direnv.enable = lib.mkEnableOption "direnv";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ direnv lorri ];

    programs = {
      bash.interactiveShellInit = ''
        eval "$(${pkgs.direnv}/bin/direnv hook bash)"
      '';
      zsh.interactiveShellInit = ''
        eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
      '';
      fish.interactiveShellInit = ''
        eval (${pkgs.direnv}/bin/direnv hook fish)
      '';
    };

    services.lorri.enable = true;
  };
}
