{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ python3 poetry python-language-server ];
}
