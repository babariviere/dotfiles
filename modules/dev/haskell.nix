{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.haskell;
  # TODO: this thing is huge to download.
  all-hies =
    import (builtins.fetchGit "https://github.com/infinisil/all-hies") { };
in {
  options.dotfiles.dev.haskell.enable = lib.mkEnableOption "haskell";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      ghc
      cabal-install
      stack
      cabal2nix
      # (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
      haskellPackages.hoogle
    ];

    home-manager.users."${dotfiles.user}".home.file.".ghci".source =
      pkgs.mutate <config/ghci> {
        hoogle = "${pkgs.haskellPackages.hoogle}/bin/hoogle";
      };
  };
}
