{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ go gopls ];

  shell.env.GOPATH = "${config.home.homeDirectory}/src";
}
