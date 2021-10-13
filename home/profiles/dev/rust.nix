{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ rustup ];
  home.sessionPath = [ "${config.xdg.dataHome}/cargo/bin" ];

  shell.env = {
    RUSTUP_HOME = "${config.xdg.dataHome}/rustup";
    CARGO_HOME = "${config.xdg.dataHome}/cargo";
  };
}
