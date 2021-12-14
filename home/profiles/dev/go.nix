{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ go gopls gomod2nix ];

  shell.env.GOPATH = "${config.home.homeDirectory}/src";
}
