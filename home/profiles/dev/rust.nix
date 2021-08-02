{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ rustup ];

  home.sessionVariables = {
    RUSTUP_HOME = "${config.xdg.dataHome}/rustup";
    CARGO_HOME = "${config.xdg.dataHome}/cargo";
  };
}