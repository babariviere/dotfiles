{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ ccls cmake conan gcc ninja ];
}
