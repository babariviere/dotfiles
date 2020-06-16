{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.haskell;
in {
  options.dotfiles.dev.haskell.enable = lib.mkEnableOption "haskell";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [
        ghc # TODO: declare version at one place
        # cabal-install
        stack
        # snack
        # cabal2nix
      ] ++ (with haskellPackages; [ hoogle brittany hlint ]);

    home-manager.users."${dotfiles.user}" = {
      xdg.configFile."brittany" = {
        source = (usrconf "brittany");
        recursive = true;
      };

      home.file.".ghci".source = pkgs.mutate (usrconf "ghci") {
        hoogle = "${pkgs.haskellPackages.hoogle}/bin/hoogle";
      };
    };
  };
}
