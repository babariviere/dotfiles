{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles;
  unstable = import <nixpkgs-unstable> { };
in {
  options.dotfiles.dev.elixir.enable = lib.mkEnableOption "elixir";

  config = lib.mkIf cfg.dev.elixir.enable {
    environment.systemPackages = with unstable; [
      erlang
      elixir
      rebar3
      inotify-tools
    ];
  };
}
