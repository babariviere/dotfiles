{ config, lib, pkgs, ... }:

with lib;
{
  options.services.dotfiles = {
    user = mkOption {
      type = types.string;
      description = "user used for services";
    };
  };
}
