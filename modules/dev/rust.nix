{ config, lib, pkgs, ... }:

let cfg = config.my.dev.rust;
in {
  options.my.dev.rust = with lib; { enable = mkEnableOption "rust"; };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ rustup ];

    env = {
      RUSTUP_HOME = "${config.hm.xdg.dataHome}/rustup";
      CARGO_HOME = "${config.hm.xdg.dataHome}/cargo";
    };
  };
}
