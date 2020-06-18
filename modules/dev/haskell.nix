{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.haskell;

  ghc' = pkgs.haskellPackages.ghcWithPackages (p:
    with p;
    [ stack hoogle ormolu hlint ]
    ++ lib.optionals (dotfiles.desktop.xmonad.enable) [
      # TODO: avoid this ?
      xmonad-contrib
      xmonad-extras
      xmonad
      xmobar
    ]);
in {
  options.dotfiles.dev.haskell.enable = lib.mkEnableOption "haskell";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ ghc' ];

    home-manager.users."${dotfiles.user}" = {
      home.file.".ghci".source = pkgs.mutate (usrconf "ghci") {
        hoogle = "${pkgs.haskellPackages.hoogle}/bin/hoogle";
      };
    };
  };
}
