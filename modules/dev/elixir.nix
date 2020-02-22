{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.elixir;
  unstable = import <nixpkgs-unstable> { };
in {
  options.dotfiles.dev.elixir.enable = lib.mkEnableOption "elixir";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with unstable; [
      erlang
      elixir
      rebar3
      inotify-tools
    ];

    environment.variables.ERL_AFLAGS = "-kernel shell_history enabled";

    home-manager.users."${dotfiles.user}" = {
      home.file = { ".iex.exs".source = <config/elixir/iex.exs>; };
    };
  };

}
