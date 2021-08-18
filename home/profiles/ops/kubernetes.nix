{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.kubectl pkgs.kubectx pkgs.kubernetes-helm ];
}
