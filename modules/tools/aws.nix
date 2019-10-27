{ config, lib, pkgs, ... }:

let
  dotfiles = config.services.dotfiles;
  cfg = dotfiles.tools.aws;
in {
  # TODO: manage aws/config aws/credentials ?
  config =
    lib.mkIf cfg.enable { environment.systemPackages = with pkgs; [ awscli ]; };
}
