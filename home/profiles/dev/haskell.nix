{ config, pkgs }:

{
  home.sessionPath = [ "${config.home.homeDirectory}/.cabal/bin" ];

  home.packages = with pkgs; [ cabal-install ];
}
