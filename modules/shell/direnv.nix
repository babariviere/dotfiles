{ config, pkgs, lib, ... }:

let cfg = config.dotfiles.shell.direnv;
in {
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
