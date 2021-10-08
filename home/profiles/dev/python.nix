{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ poetry python-language-server ];
}
