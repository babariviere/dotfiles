{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ ccls clang-tools cmake gcc ninja ];
}
