{ config, pkgs, lib, ... }:

let cfg = config.dotfiles.shell.direnv;
in {
  disabledModules = [ "services/development/lorri.nix" ];
  imports = [ <nixos-unstable/nixos/modules/services/development/lorri.nix> ];

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
