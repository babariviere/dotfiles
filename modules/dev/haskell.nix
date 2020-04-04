{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.haskell;
  all-hies = import pkgs.sources.all-hies { };
in {
  options.dotfiles.dev.haskell.enable = lib.mkEnableOption "haskell";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [
        ghc # TODO: declare version at one place
        cabal-install
        stack
        snack
        cabal2nix
        (all-hies.selection { selector = p: { inherit (p) ghc882; }; })
      ] ++ (with haskellPackages; [ hoogle brittany hlint ]);

    home-manager.users."${dotfiles.user}" = {
      xdg.configFile."brittany" = {
        source = <config/brittany>;
        recursive = true;
      };

      home.file.".ghci".source = pkgs.mutate <config/ghci> {
        hoogle = "${pkgs.haskellPackages.hoogle}/bin/hoogle";
      };
    };
  };
}
