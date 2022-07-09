{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ black python3 poetry python-language-server ];
}
