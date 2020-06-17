{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.haskell;
in {
  options.dotfiles.dev.haskell.enable = lib.mkEnableOption "haskell";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # cabal-install
      stack
      # snack
      # cabal2nix
      (haskellPackages.ghcWithPackages
        (ps: with ps; [ hoogle ormolu hlint ghcide ]))
    ];

    home-manager.users."${dotfiles.user}" = {
      home.file.".ghci".source = pkgs.mutate (usrconf "ghci") {
        hoogle = "${pkgs.haskellPackages.hoogle}/bin/hoogle";
      };
    };
  };
}
