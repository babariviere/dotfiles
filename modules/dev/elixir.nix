{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.elixir;
in {
  options.dotfiles.dev.elixir.enable = lib.mkEnableOption "elixir";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs.unstable; [
      erlang
      elixir
      rebar3
      inotify-tools
    ];

    environment.variables.ERL_AFLAGS = "-kernel shell_history enabled";

    home-manager.users."${dotfiles.user}" = {
      home.file = { ".iex.exs".source = (usrconf "elixir/iex.exs"); };
    };
  };

}
