{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ go gopls ];

  env.GOPATH = "${config.home.homeDirectory}/src";
}
