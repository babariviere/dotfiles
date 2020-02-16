{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  options.dotfiles.dev.elixir.enable = lib.mkEnableOption "elixir";

  config = lib.mkIf cfg.dev.elixir.enable {
    environment.systemPackages = with pkgs; [
      erlang
      elixir
      rebar3
      beamPackages.hex
    ];
  };
}
