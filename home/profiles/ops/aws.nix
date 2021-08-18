{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.awscli2 ];
}
