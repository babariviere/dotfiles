{ config, pkgs, lib, ... }:

let cfg = config.dotfiles.shell.direnv;
in {
  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [
      (self: super: {
        lorri = let
          src = (super.fetchFromGitHub {
            owner = "target";
            repo = "lorri";
            rev = "a0f21810c7e0476c206d63b80414b0d6111bfafb";
            sha256 = "1y2fvyhlk64idwv8y0smws92bkk51j8z4bagvvlncv3q31147i9n";
          });
        in super.callPackage src { inherit src; };
      })
    ];

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
  };
}
