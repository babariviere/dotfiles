{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ elixir elixir_ls ];
}
