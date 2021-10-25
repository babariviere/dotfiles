{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ ccls cmake gcc ninja ];
}
