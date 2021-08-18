{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.dogdns ];
}
