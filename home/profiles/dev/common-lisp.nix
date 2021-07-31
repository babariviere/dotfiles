{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.sbcl ];
}
