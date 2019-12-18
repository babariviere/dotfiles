{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  config = lib.mkIf cfg.dev.rust.enable {
    environment.systemPackages = with pkgs; [
      rustc
      rust.packages.stable.clippy
      rls
      rustfmt
      cargo
      cargo-audit
      cargo-edit
      cargo-fuzz
      cargo-outdated
    ];
  };
}
