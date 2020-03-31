{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.rust;
in {
  options.dotfiles.dev.rust.enable = lib.mkEnableOption "rust";

  config = lib.mkIf cfg.enable {
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

    home-manager.users."${dotfiles.user}" = {
      xdg.configFile = {
        "zsh/rc.d/env.rust.zsh".source = <config/rust/env.zsh>;
        "fish/rc.d/env.rust.fish".source = <config/rust/env.fish>;
      };
    };
  };
}
