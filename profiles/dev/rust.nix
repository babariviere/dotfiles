{ config, lib, pkgs, ... }: {
  home.packages = with pkgs; [ rustup ];

  env = {
    RUSTUP_HOME = "${config.hm.xdg.dataHome}/rustup";
    CARGO_HOME = "${config.hm.xdg.dataHome}/cargo";
  };
}
