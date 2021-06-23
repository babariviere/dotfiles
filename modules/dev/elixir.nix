{ config, lib, pkgs, ... }:

let cfg = config.my.dev.elixir;
in {
  options.my.dev.elixir = with lib; { enable = mkEnableOption "elixir"; };

  config =
    lib.mkIf cfg.enable { home.packages = with pkgs; [ elixir elixir_ls ]; };
}
