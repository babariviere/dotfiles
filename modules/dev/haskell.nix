{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.haskell;
  all-hies =
    import (builtins.fetchGit "https://github.com/infinisil/all-hies") { };
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      ghc
      stack
      cabal2nix
      # TODO: hie is a bit slow, switch to dante ?
      (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
      haskellPackages.hoogle
    ];

    home-manager.users."${dotfiles.user}".home.file.".ghci".source =
      <config/ghci>;
  };
}
