{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ go gopls gotools ];

  shell.env.GOPATH = "${config.home.homeDirectory}/src";
}
