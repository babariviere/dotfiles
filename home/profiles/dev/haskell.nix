{ config, pkgs }:

{
  home.sessionPath = [ "${config.home.homeDirectory}/.cabal/bin" ];

  home.packages = with pkgs; let
    ghc' = ghc.withPackages (hp: with hp; [ zlib ]);
  in [ cabal-install ghc' pkg-config zlib.dev ];
}
