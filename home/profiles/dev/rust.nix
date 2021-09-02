{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ rustup ];

  env = {
    RUSTUP_HOME = "${config.xdg.dataHome}/rustup";
    CARGO_HOME = "${config.xdg.dataHome}/cargo";
  };
}
